/* -*- Mode: c; tab-width: 8; c-basic-offset: 4; indent-tabs-mode: t; -*- */
/* Cairo - a vector graphics library with display and print output
 *
 * Copyright (C) 2009 Feynman Software
 *
 * This library is free software; you can redistribute it and/or
 * modify it either under the terms of the GNU Lesser General Public
 * License version 2.1 as published by the Free Software Foundation
 * (the "LGPL") or, at your option, under the terms of the Mozilla
 * Public License Version 1.1 (the "MPL"). If you do not alter this
 * notice, a recipient may use your version of this file under either
 * the MPL or the LGPL.
 *
 * You should have received a copy of the LGPL along with this library
 * in the file COPYING-LGPL-2.1; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * You should have received a copy of the MPL along with this library
 * in the file COPYING-MPL-1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY
 * OF ANY KIND, either express or implied. See the LGPL or the MPL for
 * the specific language governing rights and limitations.
 *
 * The Original Code is the cairo graphics library.
 *
 * The Initial Developer of the Original Code is Red Hat, Inc.
 *
 * Contributor(s):
 *        WEI Yongming <ymwei@minigui.com>
 */

#include "cairoint.h"

#include "cairo-clip-private.h"
#include "cairo-minigui.h"

#include <minigui/common.h>
#include <minigui/minigui.h>
#include <minigui/gdi.h>

#undef DEBUG_COMPOSITE

/* for older SDKs */
#ifndef SHADEBLENDCAPS
#define SHADEBLENDCAPS  120
#endif
#ifndef SB_NONE
#define SB_NONE         0x00000000
#endif

#define PELS_72DPI  ((LONG)(72. / 0.0254))

static const cairo_surface_backend_t cairo_minigui_surface_backend;

typedef struct _cairo_minigui_surface {
    cairo_surface_t base;

    cairo_format_t format;

    HDC dc;

    /* We create off-screen surfaces as DIBs or DDBs, based on what we created
     * originally */
    PBITMAP bitmap;
    cairo_bool_t is_dib;

    cairo_surface_t *image;

    cairo_rectangle_int_t extents;

    /* Initial clip bits
     * We need these kept around so that we maintain
     * whatever clip was set on the original DC at creation
     * time when cairo is asked to reset the surface clip.
     */
    RECT clip_rect;
    PCLIPRGN initial_clip_rgn;
    cairo_bool_t had_simple_clip;

    /* Surface DC flags */
    uint32_t flags;
} cairo_minigui_surface_t;

/* Surface DC flag values */
enum {
    /* If this is a surface created for printing or not */
    CAIRO_MINIGUI_SURFACE_FOR_PRINTING = (1<<0),

    /* Whether the DC is a display DC or not */
    CAIRO_MINIGUI_SURFACE_IS_DISPLAY = (1<<1),

    /* Whether we can use BitBlt with this surface */
    CAIRO_MINIGUI_SURFACE_CAN_BITBLT = (1<<2),

    /* Whether we can use AlphaBlend with this surface */
    CAIRO_MINIGUI_SURFACE_CAN_ALPHABLEND = (1<<3),

    /* Whether we can use StretchBlt with this surface */
    CAIRO_MINIGUI_SURFACE_CAN_STRETCHBLT = (1<<4),

    /* Whether we can use StretchDIBits with this surface */
    CAIRO_MINIGUI_SURFACE_CAN_STRETCHDIB = (1<<5),

    /* Whether we can use GradientFill rectangles with this surface */
    CAIRO_MINIGUI_SURFACE_CAN_RECT_GRADIENT = (1<<6),
};

/**
 * _cairo_minigui_print_gdi_error:
 * @context: context string to display along with the error
 *
 * Helper function to dump out a human readable form of the
 * current error code.
 *
 * Return value: A cairo status code for the error code
 **/
static cairo_status_t
_cairo_minigui_print_gdi_error (const char *context)
{
    fprintf (stderr, "%s: MiniGUI GDI error.\n", context);
    return _cairo_error (CAIRO_STATUS_NO_MEMORY);
}

static uint32_t
_cairo_minigui_flags_for_dc (HDC dc)
{
    uint32_t flags = CAIRO_MINIGUI_SURFACE_IS_DISPLAY;

    if (GetGDCapability (dc, GDCAP_DEPTH) > 8) {
        flags |= CAIRO_MINIGUI_SURFACE_CAN_BITBLT;
        flags |= CAIRO_MINIGUI_SURFACE_CAN_ALPHABLEND;
        flags |= CAIRO_MINIGUI_SURFACE_CAN_STRETCHBLT;
        flags |= CAIRO_MINIGUI_SURFACE_CAN_STRETCHDIB;
    }
    else {
        flags |= CAIRO_MINIGUI_SURFACE_CAN_BITBLT;
        flags |= CAIRO_MINIGUI_SURFACE_CAN_STRETCHBLT;
        flags |= CAIRO_MINIGUI_SURFACE_CAN_STRETCHDIB;
    }

    return flags;
}

/* Notes:
 *
 * Win32 alpha-understanding functions
 *
 * BitBlt - will copy full 32 bits from a 32bpp DIB to result
 *          (so it's safe to use for ARGB32->ARGB32 SOURCE blits)
 *          (but not safe going RGB24->ARGB32, if RGB24 is also represented
 *           as a 32bpp DIB, since the alpha isn't discarded!)
 *
 * AlphaBlend - if both the source and dest have alpha, even if AC_SRC_ALPHA isn't set,
 *              it will still copy over the src alpha, because the SCA value (255) will be
 *              multiplied by all the src components.
 */

static cairo_int_status_t
_cairo_minigui_save_initial_clip (HDC hdc, cairo_minigui_surface_t *surface)
{
    int clipBoxType;

    clipBoxType = GetClipBox (hdc, &surface->clip_rect);
    if (clipBoxType < 0) {
        _cairo_minigui_print_gdi_error ("cairo_minigui_surface_create");
        return _cairo_error (CAIRO_STATUS_NO_MEMORY);
    }

    surface->initial_clip_rgn = NULL;
    surface->had_simple_clip = FALSE;

    if (clipBoxType == COMPLEXREGION) {
        surface->initial_clip_rgn = CreateClipRgn ();
        if (GetClipRegion (hdc, surface->initial_clip_rgn) <= 0) {
            DestroyClipRgn (surface->initial_clip_rgn);
            surface->initial_clip_rgn = NULL;
        }
    } else {
        surface->had_simple_clip = TRUE;
    }

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_minigui_restore_initial_clip (cairo_minigui_surface_t *surface)
{
    cairo_int_status_t status = CAIRO_STATUS_SUCCESS;

    /* initial_clip_rgn will either be a real region or NULL (which means reset to no clip region) */
    SelectClipRegion (surface->dc, surface->initial_clip_rgn);

    if (surface->had_simple_clip) {
        /* then if we had a simple clip, intersect */
        ClipRectIntersect (surface->dc, &surface->clip_rect);
    }

    return status;
}

static PBITMAP create_ddb_from_dc (HDC hdc)
{
    RECT rc = {0, 0, 1, 1};
    BITMAP* ddb = (BITMAP*) calloc (sizeof (BITMAP), 1);

    if (ddb == NULL)
        return NULL;

    ddb->bmType = BMP_TYPE_NORMAL;
    ddb->bmBitsPerPixel = GetGDCapability (hdc, GDCAP_BITSPP);
    ddb->bmBytesPerPixel = GetGDCapability (hdc, GDCAP_BPP);
    ddb->bmAlpha = 0;
    ddb->bmColorKey = 0;
    ddb->bmWidth = GetGDCapability (hdc, GDCAP_HPIXEL);
    ddb->bmHeight = GetGDCapability (hdc, GDCAP_VPIXEL);
    ddb->bmBits = LockDC (hdc, &rc, NULL, NULL, (int*)&ddb->bmPitch);
    UnlockDC (hdc);

    return ddb;
}

static cairo_status_t
_create_dc_and_bitmap (cairo_minigui_surface_t *surface,
                       HDC                    original_dc,
                       cairo_format_t         format,
                       int                    width,
                       int                    height,
                       unsigned char        **bits_out,
                       int                   *rowstride_out)
{
    cairo_status_t status;
    int i;

    surface->dc = HDC_INVALID;
    surface->bitmap = NULL;
    surface->is_dib = FALSE;

    width = (width == 0) ? 1 : width;
    height = (height == 0) ? 1 : height;

    switch (format) {
    /* We can't create real RGB24 bitmaps because something seems to
     * break if we do, especially if we don't set up an image
     * fallback.  It could be a bug with using a 24bpp pixman image
     * (and creating one with masks).  So treat them like 32bpp.
     * Note: This causes problems when using BitBlt/AlphaBlend/etc!
     * see end of file.
     */
    case CAIRO_FORMAT_RGB24:
        surface->dc = CreateMemDC (width, height,
                        32, MEMDC_FLAG_SWSURFACE,
                        0x00FF0000, 0x0000FF00, 0x000000FF, 0x00000000);
        break;

    case CAIRO_FORMAT_ARGB32:
        surface->dc = CreateMemDC (width, height,
                        32, MEMDC_FLAG_SWSURFACE,
                        0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000);
        break;
        
    case CAIRO_FORMAT_A8:
        surface->dc = CreateMemDC (width, height,
                        8, MEMDC_FLAG_SWSURFACE,
                        0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000);
        {
            GAL_Color cmap [256];
            for (i = 0; i < 256; i++) {
                cmap[i].r = i;
                cmap[i].g = i;
                cmap[i].b = i;
                cmap[i].a = 0;
            }
            SetPalette (surface->dc, 0, 256, cmap);
        }
        break;

    case CAIRO_FORMAT_A1:
        goto FAIL;
    }

    if (surface->dc == HDC_INVALID)
        goto FAIL;

    surface->bitmap = create_ddb_from_dc (surface->dc);
    if (!surface->bitmap)
        goto FAIL;

    surface->is_dib = TRUE;

    if (bits_out)
        *bits_out = surface->bitmap->bmBits;

    if (rowstride_out)
        *rowstride_out = surface->bitmap->bmPitch;

    surface->flags = _cairo_minigui_flags_for_dc (surface->dc);

    return CAIRO_STATUS_SUCCESS;

FAIL:
    status = _cairo_minigui_print_gdi_error ("_create_dc_and_bitmap");

    if (surface->bitmap) {
        free (surface->bitmap);
        surface->bitmap = NULL;
    }

    if (surface->dc && HDC_INVALID != surface->dc) {
        DeleteMemDC (surface->dc);
        surface->dc = HDC_INVALID;
    }

    return status;
}

static cairo_surface_t *
_cairo_minigui_surface_create_for_dc (HDC           original_dc,
                                    cairo_format_t  format,
                                    int             width,
                                    int             height)
{
    cairo_status_t status;
    cairo_minigui_surface_t *surface;
    unsigned char *bits;
    int rowstride;

    surface = malloc (sizeof (cairo_minigui_surface_t));
    if (surface == NULL)
        return _cairo_surface_create_in_error (_cairo_error (CAIRO_STATUS_NO_MEMORY));

    width = (width <= 0) ? GetGDCapability(original_dc, GDCAP_HPIXEL) : width;
    height = (height <= 0) ? GetGDCapability(original_dc, GDCAP_VPIXEL) : height; 

    status = _create_dc_and_bitmap (surface, original_dc, format,
                                    width, height,
                                    &bits, &rowstride);
    if (status)
        goto FAIL;

    surface->image = cairo_image_surface_create_for_data (bits, format,
                                                          width, height, rowstride);
    status = surface->image->status;
    if (status)
        goto FAIL;

    surface->format = format;

    surface->clip_rect.left = 0;
    surface->clip_rect.top = 0;
    surface->clip_rect.right = width;
    surface->clip_rect.bottom = height;

    surface->initial_clip_rgn = NULL;
    surface->had_simple_clip = FALSE;

    surface->extents.x = 0;
    surface->extents.y = 0;
    surface->extents.width = width;
    surface->extents.height = height;

    _cairo_surface_init (&surface->base, &cairo_minigui_surface_backend,
                         _cairo_content_from_format (format));

    return &surface->base;

FAIL:
    if (surface->bitmap) {
        free (surface->bitmap);
        DeleteMemDC (surface->dc);
    }

    free (surface);

    return _cairo_surface_create_in_error (status);
}

static cairo_surface_t *
_cairo_minigui_surface_create_similar_internal (void            *abstract_src,
                                              cairo_content_t content,
                                              int             width,
                                              int             height,
                                              cairo_bool_t   force_dib)
{
    cairo_minigui_surface_t *src = abstract_src;
    cairo_format_t format = _cairo_format_from_content (content);
    cairo_surface_t *new_surf = NULL;

    /* We force a DIB always if:
     * - we need alpha; or
     * - the parent is a DIB; or
     * - the parent is for printing (because we don't care about the bit depth at that point)
     *
     * We also might end up with a DIB even if a DDB is requested if DDB creation failed
     * due to out of memory.
     */
    if (src->is_dib ||
            (content & CAIRO_CONTENT_ALPHA)) {
        force_dib = TRUE;
    }

    if (!force_dib) {
        /* try to create a ddb */
        new_surf = cairo_minigui_surface_create_with_ddb (src->dc, CAIRO_FORMAT_RGB24, width, height);

        if (new_surf->status != CAIRO_STATUS_SUCCESS)
            new_surf = NULL;
    }

    if (new_surf == NULL) {
        new_surf = _cairo_minigui_surface_create_for_dc (src->dc, format, width, height);
    }

    return new_surf;
}

static cairo_surface_t *
_cairo_minigui_surface_create_similar (void            *abstract_src,
                                     cairo_content_t content,
                                     int             width,
                                     int             height)
{
    return _cairo_minigui_surface_create_similar_internal (abstract_src, content, width, height, FALSE);
}

static cairo_status_t
_cairo_minigui_surface_clone_similar (void *abstract_surface,
                                    cairo_surface_t *src,
                                    int src_x,
                                    int src_y,
                                    int width,
                                    int height,
                                    int *clone_offset_x,
                                    int *clone_offset_y,
                                    cairo_surface_t **clone_out)
{
    cairo_content_t src_content;
    cairo_surface_t *new_surface;
    cairo_status_t status;
    cairo_surface_pattern_t pattern;

    src_content = cairo_surface_get_content(src);
    new_surface =
        _cairo_minigui_surface_create_similar_internal (abstract_surface,
                                                      src_content,
                                                      width, height,
                                                      FALSE);
    if (new_surface == NULL)
        return CAIRO_INT_STATUS_UNSUPPORTED;

    status = new_surface->status;
    if (status)
        return status;

    _cairo_pattern_init_for_surface (&pattern, src);

    status = _cairo_surface_composite (CAIRO_OPERATOR_SOURCE,
                                       &pattern.base,
                                       NULL,
                                       new_surface,
                                       src_x, src_y,
                                       0, 0,
                                       0, 0,
                                       width, height);

    _cairo_pattern_fini (&pattern.base);

    if (status == CAIRO_STATUS_SUCCESS) {
        *clone_offset_x = src_x;
        *clone_offset_y = src_y;
        *clone_out = new_surface;
    } else
        cairo_surface_destroy (new_surface);

    return status;
}

static cairo_status_t
_cairo_minigui_surface_finish (void *abstract_surface)
{
    cairo_minigui_surface_t *surface = abstract_surface;

    if (surface->image)
        cairo_surface_destroy (surface->image);

    /* If we created the Bitmap and DC, destroy them */
    if (surface->bitmap) {
        free (surface->bitmap);
        DeleteMemDC (surface->dc);
    } else {
        _cairo_minigui_restore_initial_clip (surface);
    }

    if (surface->initial_clip_rgn)
        DestroyClipRgn (surface->initial_clip_rgn);

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_minigui_surface_get_subimage (cairo_minigui_surface_t  *surface,
                                   int                     x,
                                   int                     y,
                                   int                     width,
                                   int                     height,
                                   cairo_minigui_surface_t **local_out)
{
    cairo_minigui_surface_t *local;
    cairo_int_status_t status;
    cairo_content_t content = _cairo_content_from_format (surface->format);

    local =
        (cairo_minigui_surface_t *) _cairo_minigui_surface_create_similar_internal
        (surface, content, width, height, TRUE);
    if (local == NULL)
        return CAIRO_INT_STATUS_UNSUPPORTED;
    if (local->base.status)
        return local->base.status;

    status = CAIRO_INT_STATUS_UNSUPPORTED;

    /* Only BitBlt if the source surface supports it. */
    if (surface->flags & CAIRO_MINIGUI_SURFACE_CAN_BITBLT)
    {
        BitBlt (surface->dc, x, y, width, height,
                local->dc, 0, 0,
                0);
        status = CAIRO_STATUS_SUCCESS;
    }

    if (status) {
        FillBox (local->dc, 0, 0, width, height);
    }

    *local_out = local;

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_minigui_surface_acquire_source_image (void                    *abstract_surface,
                                           cairo_image_surface_t  **image_out,
                                           void                   **image_extra)
{
    cairo_minigui_surface_t *surface = abstract_surface;
    cairo_minigui_surface_t *local = NULL;
    cairo_status_t status;

    if (surface->image) {
        *image_out = (cairo_image_surface_t *)surface->image;
        *image_extra = NULL;

        return CAIRO_STATUS_SUCCESS;
    }

    status = _cairo_minigui_surface_get_subimage (abstract_surface, 0, 0,
                                                surface->extents.width,
                                                surface->extents.height, &local);
    if (status)
        return status;

    *image_out = (cairo_image_surface_t *)local->image;
    *image_extra = local;

    return CAIRO_STATUS_SUCCESS;
}

static void
_cairo_minigui_surface_release_source_image (void                   *abstract_surface,
                                           cairo_image_surface_t  *image,
                                           void                   *image_extra)
{
    cairo_minigui_surface_t *local = image_extra;

    if (local)
        cairo_surface_destroy ((cairo_surface_t *)local);
}

static cairo_status_t
_cairo_minigui_surface_acquire_dest_image (void                    *abstract_surface,
                                         cairo_rectangle_int_t   *interest_rect,
                                         cairo_image_surface_t  **image_out,
                                         cairo_rectangle_int_t   *image_rect,
                                         void                   **image_extra)
{
    cairo_minigui_surface_t *surface = abstract_surface;
    cairo_minigui_surface_t *local = NULL;
    cairo_status_t status;
    //RECT clip_box;
    int x1, y1, x2, y2;

    if (surface->image) {

        image_rect->x = 0;
        image_rect->y = 0;
        image_rect->width = surface->extents.width;
        image_rect->height = surface->extents.height;

        *image_out = (cairo_image_surface_t *)surface->image;
        *image_extra = NULL;

        return CAIRO_STATUS_SUCCESS;
    }

/*    if (GetClipBox (surface->dc, &clip_box) < 0)
        return _cairo_minigui_print_gdi_error ("_cairo_minigui_surface_acquire_dest_image");

	printf(":::::::::::: %s:%d, surface=%p, hdc=%p, clip_box=%d,%d,%d,%d\n",__FUNCTION__, __LINE__, surface, surface->dc, clip_box.left, clip_box.top, RECTW(clip_box), RECTH(clip_box));

#if 1
    x1 = clip_box.left;
    x2 = clip_box.right;
    y1 = clip_box.top;
    y2 = clip_box.bottom;
#else
	x1 = y1 = 0;
	x2 = RECTW(clip_box);
	y2 = RECTH(clip_box);
#endif*/
	x1 = surface->extents.x;
	y1 = surface->extents.y;
	x2 = x1 + surface->extents.width;
	y2 = y1 + surface->extents.height;

    if (interest_rect->x > x1)
        x1 = interest_rect->x;
    if (interest_rect->y > y1)
        y1 = interest_rect->y;
    if ((int) (interest_rect->x + interest_rect->width) < x2)
        x2 = interest_rect->x + interest_rect->width;
    if ((int) (interest_rect->y + interest_rect->height) < y2)
        y2 = interest_rect->y + interest_rect->height;

    if (x1 >= x2 || y1 >= y2) {
        *image_out = NULL;
        *image_extra = NULL;

        return CAIRO_STATUS_SUCCESS;
    }

    status = _cairo_minigui_surface_get_subimage (abstract_surface,
                                                x1, y1, x2 - x1, y2 - y1,
                                                &local);
    if (status)
        return status;

    *image_out = (cairo_image_surface_t *)local->image;
    *image_extra = local;

    image_rect->x = x1;
    image_rect->y = y1;
    image_rect->width = x2 - x1;
    image_rect->height = y2 - y1;

    return CAIRO_STATUS_SUCCESS;
}

static void
_cairo_minigui_surface_release_dest_image (void                    *abstract_surface,
                                         cairo_rectangle_int_t   *interest_rect,
                                         cairo_image_surface_t   *image,
                                         cairo_rectangle_int_t   *image_rect,
                                         void                    *image_extra)
{
    cairo_minigui_surface_t *surface = abstract_surface;
    cairo_minigui_surface_t *local = image_extra;

    if (!local)
        return;

    BitBlt (local->dc, 0, 0, image_rect->width, image_rect->height,
            surface->dc, image_rect->x, image_rect->y, 
            0);

    cairo_surface_destroy ((cairo_surface_t *)local);
}

static cairo_int_status_t
_composite_alpha_blend (cairo_minigui_surface_t *dst,
                        cairo_minigui_surface_t *src,
                        int                    alpha,
                        int                    src_x,
                        int                    src_y,
                        int                    src_w,
                        int                    src_h,
                        int                    dst_x,
                        int                    dst_y,
                        int                    dst_w,
                        int                    dst_h)
{
    if (!(dst->flags & CAIRO_MINIGUI_SURFACE_CAN_ALPHABLEND))
        return CAIRO_INT_STATUS_UNSUPPORTED;
    if (src->format == CAIRO_FORMAT_RGB24 && dst->format == CAIRO_FORMAT_ARGB32)
        return CAIRO_INT_STATUS_UNSUPPORTED;

    	//call bitblt directly
	if(src_w == dst_w && src_h == dst_h)
	{
        SetMemDCColorKey (src->dc, MEMDC_FLAG_SRCCOLORKEY, RGBA2Pixel(src->dc, 0, 0, 0, 0));
        SetMemDCAlpha (src->dc, MEMDC_FLAG_SRCALPHA, alpha);
		BitBlt(src->dc, src_x, src_y, src_w, src_h, dst->dc, dst_x, dst_y, 0);
	}
	else
	{
	
		if(GetGDCapability(src->dc, GDCAP_BPP) == GetGDCapability(dst->dc, GDCAP_BPP))
		{
            SetMemDCColorKey (src->dc, MEMDC_FLAG_SRCCOLORKEY, RGBA2Pixel(src->dc, 0, 0, 0, 0));
            SetMemDCAlpha (src->dc, MEMDC_FLAG_SRCALPHA, alpha);
		    StretchBlt (src->dc, src_x, src_y, src_w, src_h,
        	         dst->dc, dst_x, dst_y, dst_w, dst_h,
            	     0);
		}
		else
		{
			//create a compatible dc with dst, and its size is same as src
			HDC hmemdc = CreateCompatibleDCEx(dst->dc, src_w, src_h);
			//copy src to hmemdc
			BitBlt(src->dc, src_x, src_y, src_w, src_h, hmemdc, 0, 0, 0);
            SetMemDCColorKey (hmemdc, MEMDC_FLAG_SRCCOLORKEY, RGBA2Pixel(hmemdc, 0, 0, 0, 0));
            SetMemDCAlpha (hmemdc, MEMDC_FLAG_SRCALPHA, alpha);

			StretchBlt(hmemdc, 0, 0, src_w, src_h, dst->dc, dst_x, dst_y, dst_w, dst_h, 0);
			DeleteMemDC(hmemdc);
		}
	}

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_minigui_surface_composite_inner (cairo_minigui_surface_t *src,
                                      cairo_image_surface_t *src_image,
                                      cairo_minigui_surface_t *dst,
                                      cairo_rectangle_int_t src_extents,
                                      cairo_rectangle_int_t src_r,
                                      cairo_rectangle_int_t dst_r,
                                      int alpha,
                                      cairo_bool_t needs_alpha,
                                      cairo_bool_t needs_scale)
{
    /* Then do BitBlt, StretchDIBits, StretchBlt, AlphaBlend, or MaskBlt */
    if (src_image) {
        if (needs_alpha || needs_scale)
            return CAIRO_INT_STATUS_UNSUPPORTED;

        if (dst->flags & CAIRO_MINIGUI_SURFACE_CAN_STRETCHBLT) {
            MYBITMAP dib;
            HDC mem_dc;

            dib.flags = MYBMP_TYPE_RGB | MYBMP_FLOW_DOWN | MYBMP_RGBSIZE_4;
            dib.frames = 1;
            dib.depth = 32;
            dib.alpha = 0;
            dib.transparent = 0;
            dib.w = src_image->width;
            dib.h = src_image->height;
            dib.pitch = dib.w * 4;
            dib.size = dib.pitch * dib.h;
            dib.bits = src_image->data;

            mem_dc = CreateMemDCFromMyBitmap (&dib, NULL);
            if (mem_dc == HDC_INVALID)
		        return _cairo_minigui_print_gdi_error ("_cairo_win32_surface_composite(StretchDIBits)");

            StretchBlt (mem_dc, 
                                src_r.x, src_r.y,
                                src_r.width, src_r.height,
                                dst->dc,
                                dst_r.x, dst_r.y,
                                dst_r.width, dst_r.height,
                                0);
            DeleteMemDC (mem_dc);
        }
    } else if (!needs_alpha) {
        /* BitBlt or StretchBlt? */
        if (!needs_scale && (dst->flags & CAIRO_MINIGUI_SURFACE_CAN_BITBLT)) {
            BitBlt (
                        src->dc,
                        src_r.x, src_r.y,
                        dst_r.width, dst_r.height,
                        dst->dc,
                        dst_r.x, dst_r.y,
                        0);
        } else if (dst->flags & CAIRO_MINIGUI_SURFACE_CAN_STRETCHBLT) {
            /* StretchBlt? */
            /* XXX check if we want HALFTONE, based on the src filter */
            StretchBlt (
                        src->dc,
                        src_r.x, src_r.y,
                        src_r.width, src_r.height,
                        dst->dc,
                        dst_r.x, dst_r.y,
                        dst_r.width, dst_r.height,
                        0);
        }
    } else if (needs_alpha && !needs_scale) {
          return _composite_alpha_blend (dst, src, alpha,
                                       src_r.x, src_r.y, src_r.width, src_r.height,
                                       dst_r.x, dst_r.y, dst_r.width, dst_r.height);
    }

    return CAIRO_STATUS_SUCCESS;
}

/* from pixman-private.h */
#define MOD(a,b) ((a) < 0 ? ((b) - ((-(a) - 1) % (b))) - 1 : (a) % (b))

static cairo_int_status_t
_cairo_minigui_surface_composite (cairo_operator_t        op,
                                cairo_pattern_t               *pattern,
                                cairo_pattern_t                *mask_pattern,
                                void                        *abstract_dst,
                                int                        src_x,
                                int                        src_y,
                                int                        mask_x,
                                int                        mask_y,
                                int                        dst_x,
                                int                        dst_y,
                                unsigned int                width,
                                unsigned int                height)
{
    cairo_minigui_surface_t *dst = abstract_dst;
    cairo_minigui_surface_t *src;
    cairo_surface_pattern_t *src_surface_pattern;
    int alpha;
    double scalex, scaley;
    cairo_fixed_t x0_fixed, y0_fixed;
    cairo_int_status_t status;

    cairo_bool_t needs_alpha, needs_scale, needs_repeat;
    cairo_image_surface_t *src_image = NULL;

    cairo_format_t src_format;
    cairo_rectangle_int_t src_extents;

    cairo_rectangle_int_t src_r = { src_x, src_y, width, height };
    cairo_rectangle_int_t dst_r = { dst_x, dst_y, width, height };

#ifdef DEBUG_COMPOSITE
    fprintf (stderr, "+++ composite: %d %p %p %p [%d %d] [%d %d] [%d %d] %dx%d\n",
             op, pattern, mask_pattern, abstract_dst, src_x, src_y, mask_x, mask_y, dst_x, dst_y, width, height);
#endif

    /* If the destination can't do any of these, then
     * we may as well give up, since this is what we'll
     * look to for optimization.
     */
    if ((dst->flags & (CAIRO_MINIGUI_SURFACE_CAN_BITBLT |
                       CAIRO_MINIGUI_SURFACE_CAN_ALPHABLEND |
                       CAIRO_MINIGUI_SURFACE_CAN_STRETCHBLT |
                       CAIRO_MINIGUI_SURFACE_CAN_STRETCHDIB))
        == 0)
    {
        goto UNSUPPORTED;
    }

    if (pattern->type != CAIRO_PATTERN_TYPE_SURFACE)
        goto UNSUPPORTED;

    if (pattern->extend != CAIRO_EXTEND_NONE &&
        pattern->extend != CAIRO_EXTEND_REPEAT)
        goto UNSUPPORTED;

    if (mask_pattern) {
        /* FIXME: When we fully support RENDER style 4-channel
         * masks we need to check r/g/b != 1.0.
         */
        if (mask_pattern->type != CAIRO_PATTERN_TYPE_SOLID)
            return CAIRO_INT_STATUS_UNSUPPORTED;

        alpha = ((cairo_solid_pattern_t *)mask_pattern)->color.alpha_short >> 8;
    } else {
        alpha = 255;
    }

    src_surface_pattern = (cairo_surface_pattern_t *)pattern;
    src = (cairo_minigui_surface_t *)src_surface_pattern->surface;

    if (src->base.type == CAIRO_SURFACE_TYPE_IMAGE &&
        dst->flags & (CAIRO_MINIGUI_SURFACE_CAN_STRETCHDIB))
    {
        /* In some very limited cases, we can use StretchDIBits to draw
         * an image surface directly:
         *  - source is CAIRO_FORMAT_ARGB32
         *  - dest is CAIRO_FORMAT_ARGB32
         *  - alpha is 255
         *  - operator is SOURCE or OVER
         *  - image stride is 4*width
         */
        src_image = (cairo_image_surface_t*) src;

        if (src_image->format != CAIRO_FORMAT_RGB24 ||
            dst->format != CAIRO_FORMAT_RGB24 ||
            alpha != 255 ||
            (op != CAIRO_OPERATOR_SOURCE && op != CAIRO_OPERATOR_OVER) ||
            src_image->stride != (src_image->width * 4))
        {
            goto UNSUPPORTED;
        }

        src_format = src_image->format;
        src_extents.x = 0;
        src_extents.y = 0;
        src_extents.width = src_image->width;
        src_extents.height = src_image->height;
    } else if (src->base.backend != dst->base.backend) {
        goto UNSUPPORTED;
    } else {
        src_format = src->format;
        src_extents = src->extents;
    }


#ifdef DEBUG_COMPOSITE
    fprintf (stderr, "Before check: src size: (%d %d) xy [%d %d] -> dst [%d %d %d %d] {srcmat %f %f %f %f}\n",
             src_extents.width, src_extents.height,
             src_x, src_y,
             dst_x, dst_y, width, height,
             pattern->matrix.x0, pattern->matrix.y0, pattern->matrix.xx, pattern->matrix.yy);
#endif

    /* We can only use GDI functions if the source and destination rectangles
     * are on integer pixel boundaries.  Figure that out here.
     */
    x0_fixed = _cairo_fixed_from_double(pattern->matrix.x0 / pattern->matrix.xx);
    y0_fixed = _cairo_fixed_from_double(pattern->matrix.y0 / pattern->matrix.yy);

    if (pattern->matrix.yx != 0.0 ||
        pattern->matrix.xy != 0.0 ||
        !_cairo_fixed_is_integer(x0_fixed) ||
        !_cairo_fixed_is_integer(y0_fixed))
    {
        goto UNSUPPORTED;
    }

    scalex = pattern->matrix.xx;
    scaley = pattern->matrix.yy;

    src_r.x += _cairo_fixed_integer_part(x0_fixed);
    src_r.y += _cairo_fixed_integer_part(y0_fixed);

    /* Success, right? */
    if (scalex == 0.0 || scaley == 0.0)
        return CAIRO_STATUS_SUCCESS;

    if (scalex != 1.0 || scaley != 1.0)
        goto UNSUPPORTED;

    /* If the src coordinates are outside of the source surface bounds,
     * we have to fix them up, because this is an error for the GDI
     * functions.
     */

#ifdef DEBUG_COMPOSITE
    fprintf (stderr, "before: [%d %d %d %d] -> [%d %d %d %d]\n",
             src_r.x, src_r.y, src_r.width, src_r.height,
             dst_r.x, dst_r.y, dst_r.width, dst_r.height);
    fflush (stderr);
#endif

    /* If the src recangle doesn't wholly lie within the src extents,
     * fudge things.  We really need to do fixup on the unpainted
     * region -- e.g. the SOURCE operator is broken for areas outside
     * of the extents, because it won't clear that area to transparent
     * black.
     */

    if (pattern->extend != CAIRO_EXTEND_REPEAT) {
        needs_repeat = FALSE;

        /* If the src rect and the extents of the source image don't overlap at all,
         * we can't do anything useful here.
         */
        if (src_r.x > src_extents.width || src_r.y > src_extents.height ||
            (src_r.x + src_r.width) < 0 || (src_r.y + src_r.height) < 0)
        {
            if (op == CAIRO_OPERATOR_OVER)
                return CAIRO_STATUS_SUCCESS;
            goto UNSUPPORTED;
        }

        if (src_r.x < 0) {
            src_r.width += src_r.x;
            src_r.x = 0;

            dst_r.width += src_r.x;
            dst_r.x -= src_r.x;
        }

        if (src_r.y < 0) {
            src_r.height += src_r.y;
            src_r.y = 0;

            dst_r.height += dst_r.y;
            dst_r.y -= src_r.y;
        }

        if (src_r.x + src_r.width > src_extents.width) {
            src_r.width = src_extents.width - src_r.x;
            dst_r.width = src_r.width;
        }

        if (src_r.y + src_r.height > src_extents.height) {
            src_r.height = src_extents.height - src_r.y;
            dst_r.height = src_r.height;
        }
    } else {
        needs_repeat = TRUE;
    }

    /*
     * Operations that we can do:
     *
     *  RGB OVER  RGB -> BitBlt (same as SOURCE)
     *  RGB OVER ARGB -> UNSUPPORTED (AlphaBlend treats this as a BitBlt, even with SCA 255 and no AC_SRC_ALPHA)
     * ARGB OVER ARGB -> AlphaBlend, with AC_SRC_ALPHA
     * ARGB OVER  RGB -> AlphaBlend, with AC_SRC_ALPHA; we'll have junk in the dst A byte
     * 
     *  RGB OVER  RGB + mask -> AlphaBlend, no AC_SRC_ALPHA
     *  RGB OVER ARGB + mask -> UNSUPPORTED
     * ARGB OVER ARGB + mask -> AlphaBlend, with AC_SRC_ALPHA
     * ARGB OVER  RGB + mask -> AlphaBlend, with AC_SRC_ALPHA; junk in the dst A byte
     * 
     *  RGB SOURCE  RGB -> BitBlt
     *  RGB SOURCE ARGB -> UNSUPPORTED (AlphaBlend treats this as a BitBlt, even with SCA 255 and no AC_SRC_ALPHA)
     * ARGB SOURCE ARGB -> BitBlt
     * ARGB SOURCE  RGB -> BitBlt
     * 
     *  RGB SOURCE  RGB + mask -> unsupported
     *  RGB SOURCE ARGB + mask -> unsupported
     * ARGB SOURCE ARGB + mask -> unsupported
     * ARGB SOURCE  RGB + mask -> unsupported
     */

    /*
     * Figure out what action to take.
     */
    if (op == CAIRO_OPERATOR_OVER) {
        if (alpha == 0)
            return CAIRO_STATUS_SUCCESS;

        if (src_format == dst->format) {
            if (alpha == 255 && src_format == CAIRO_FORMAT_RGB24) {
                needs_alpha = FALSE;
            } else {
                needs_alpha = TRUE;
            }
        } else if (src_format == CAIRO_FORMAT_ARGB32 &&
                   dst->format == CAIRO_FORMAT_RGB24)
        {
            needs_alpha = TRUE;
        } else {
            goto UNSUPPORTED;
        }
    } else if (alpha == 255 && op == CAIRO_OPERATOR_SOURCE) {
        if ((src_format == dst->format) ||
            (src_format == CAIRO_FORMAT_ARGB32 && dst->format == CAIRO_FORMAT_RGB24))
        {
            needs_alpha = FALSE;
        } else {
            goto UNSUPPORTED;
        }
    } else {
        goto UNSUPPORTED;
    }

    if (scalex == 1.0 && scaley == 1.0) {
        needs_scale = FALSE;
    } else {
        /* Should never be reached until we turn StretchBlt back on */
        needs_scale = TRUE;
    }

#ifdef DEBUG_COMPOSITE
    fprintf (stderr, "action: [%d %d %d %d] -> [%d %d %d %d]\n",
             src_r.x, src_r.y, src_r.width, src_r.height,
             dst_r.x, dst_r.y, dst_r.width, dst_r.height);
    fflush (stderr);
#endif

    /* If we need to repeat, we turn the repeated blit into
     * a bunch of piece-by-piece blits.
     */
    if (needs_repeat) {
        cairo_rectangle_int_t piece_src_r, piece_dst_r;
        uint32_t rendered_width = 0, rendered_height = 0;
        uint32_t to_render_height, to_render_width;
        int32_t piece_x, piece_y;
        int32_t src_start_x = MOD(src_r.x, src_extents.width);
        int32_t src_start_y = MOD(src_r.y, src_extents.height);

        if (needs_scale)
            goto UNSUPPORTED;

        /* If both the src and dest have an image, we may as well fall
         * back, because it will be faster than our separate blits.
         * Our blit code will be fastest when the src is a DDB and the
         * destination is a DDB.
         */
        if ((src_image || src->image) && dst->image)
            goto UNSUPPORTED;

        /* If the src is not a bitmap but an on-screen (or unknown)
         * DC, chances are that fallback will be faster.
         */
        if (src->bitmap == NULL)
            goto UNSUPPORTED;

        /* If we can use PatBlt, just do so */
        if (!src_image && !needs_alpha)
        {
            /* Set up the brush with our bitmap */
            SetBrushType (dst->dc, BT_TILED);
            SetBrushInfo (dst->dc, src->bitmap, NULL);
            
            /* SetBrushOrigin sets the coordinates in the destination DC of where the
             * pattern should start.
             */
            SetBrushOrigin (dst->dc, dst_r.x - src_start_x, dst_r.y - src_start_y);

            FillBox (dst->dc, dst_r.x, dst_r.y, dst_r.width, dst_r.height);

            return CAIRO_STATUS_SUCCESS;
        }

        /* If we were not able to use PatBlt, then manually expand out the blit */

        /* Arbitrary factor; we think that going through
         * fallback will be faster if we have to do more
         * than this amount of blits in either direction.
         */
        if (dst_r.width / src_extents.width > 5 ||
            dst_r.height / src_extents.height > 5)
            goto UNSUPPORTED;

        for (rendered_height = 0;
             rendered_height < dst_r.height;
             rendered_height += to_render_height)
        {
            piece_y = (src_start_y + rendered_height) % src_extents.height;
            to_render_height = src_extents.height - piece_y;

            if (rendered_height + to_render_height > dst_r.height)
                to_render_height = dst_r.height - rendered_height;

            for (rendered_width = 0;
                 rendered_width < dst_r.width;
                 rendered_width += to_render_width)
            {
                piece_x = (src_start_x + rendered_width) % src_extents.width;
                to_render_width = src_extents.width - piece_x;

                if (rendered_width + to_render_width > dst_r.width)
                    to_render_width = dst_r.width - rendered_width;

                piece_src_r.x = piece_x;
                piece_src_r.y = piece_y;
                piece_src_r.width = to_render_width;
                piece_src_r.height = to_render_height;

                piece_dst_r.x = dst_r.x + rendered_width;
                piece_dst_r.y = dst_r.y + rendered_height;
                piece_dst_r.width = to_render_width;
                piece_dst_r.height = to_render_height;

                status = _cairo_minigui_surface_composite_inner (src, src_image, dst,
                                                               src_extents, piece_src_r, piece_dst_r,
                                                               alpha, needs_alpha, needs_scale);
                if (status != CAIRO_STATUS_SUCCESS) {
                    /* Uh oh.  If something failed, and it's the first
                     * piece, then we can jump to UNSUPPORTED. 
                     * Otherwise, this is bad times, because part of the
                     * rendering was already done. */
                    if (rendered_width == 0 &&
                        rendered_height == 0)
                    {
                        goto UNSUPPORTED;
                    }

                    return status;
                }
            }
        }
    } else {
        status = _cairo_minigui_surface_composite_inner (src, src_image, dst,
                                                       src_extents, src_r, dst_r,
                                                       alpha, needs_alpha, needs_scale);
    }

    if (status == CAIRO_STATUS_SUCCESS)
        return status;

UNSUPPORTED:
    /* Fall back to image surface directly, if this is a DIB surface */
    if (dst->image) {
        return dst->image->backend->composite (op, pattern, mask_pattern,
                                               dst->image,
                                               src_x, src_y,
                                               mask_x, mask_y,
                                               dst_x, dst_y,
                                               width, height);
    }

    return CAIRO_INT_STATUS_UNSUPPORTED;
}

/* This big function tells us how to optimize operators for the
 * case of solid destination and constant-alpha source
 *
 * Note: This function needs revisiting if we add support for
 *       super-luminescent colors (a == 0, r,g,b > 0)
 */
static enum { DO_CLEAR, DO_SOURCE, DO_NOTHING, DO_UNSUPPORTED }
categorize_solid_dest_operator (cairo_operator_t op,
                                unsigned short   alpha)
{
    enum { SOURCE_TRANSPARENT, SOURCE_LIGHT, SOURCE_SOLID, SOURCE_OTHER } source;

    if (alpha >= 0xff00)
        source = SOURCE_SOLID;
    else if (alpha < 0x100)
        source = SOURCE_TRANSPARENT;
    else
        source = SOURCE_OTHER;

    switch (op) {
    case CAIRO_OPERATOR_CLEAR:    /* 0                 0 */
    case CAIRO_OPERATOR_OUT:      /* 1 - Ab            0 */
        return DO_CLEAR;
        break;

    case CAIRO_OPERATOR_SOURCE:   /* 1                 0 */
    case CAIRO_OPERATOR_IN:       /* Ab                0 */
        return DO_SOURCE;
        break;

    case CAIRO_OPERATOR_OVER:     /* 1            1 - Aa */
    case CAIRO_OPERATOR_ATOP:     /* Ab           1 - Aa */
        if (source == SOURCE_SOLID)
            return DO_SOURCE;
        else if (source == SOURCE_TRANSPARENT)
            return DO_NOTHING;
        else
            return DO_UNSUPPORTED;
        break;

    case CAIRO_OPERATOR_DEST_OUT: /* 0            1 - Aa */
    case CAIRO_OPERATOR_XOR:      /* 1 - Ab       1 - Aa */
        if (source == SOURCE_SOLID)
            return DO_CLEAR;
        else if (source == SOURCE_TRANSPARENT)
            return DO_NOTHING;
        else
            return DO_UNSUPPORTED;
            break;

    case CAIRO_OPERATOR_DEST:     /* 0                 1 */
    case CAIRO_OPERATOR_DEST_OVER:/* 1 - Ab            1 */
    case CAIRO_OPERATOR_SATURATE: /* min(1,(1-Ab)/Aa)  1 */
        return DO_NOTHING;
        break;

    case CAIRO_OPERATOR_DEST_IN:  /* 0                Aa */
    case CAIRO_OPERATOR_DEST_ATOP:/* 1 - Ab           Aa */
        if (source == SOURCE_SOLID)
            return DO_NOTHING;
        else if (source == SOURCE_TRANSPARENT)
            return DO_CLEAR;
        else
            return DO_UNSUPPORTED;
        break;

    case CAIRO_OPERATOR_ADD:          /* 1                1 */
        if (source == SOURCE_TRANSPARENT)
            return DO_NOTHING;
        else
            return DO_UNSUPPORTED;
        break;
    }

    ASSERT_NOT_REACHED;
    return DO_UNSUPPORTED;
}

static cairo_int_status_t
_cairo_minigui_surface_fill_rectangles (void                        *abstract_surface,
                                      cairo_operator_t                op,
                                      const cairo_color_t        *color,
                                      cairo_rectangle_int_t        *rects,
                                      int                        num_rects)
{
    cairo_minigui_surface_t *surface = abstract_surface;
    RGB new_color;
    int i;

    /* XXXperf If it's not RGB24, we need to do a little more checking
     * to figure out when we can use GDI.  We don't have that checking
     * anywhere at the moment, so just bail and use the fallback
     * paths. */
    if (surface->format != CAIRO_FORMAT_RGB24)
        return CAIRO_INT_STATUS_UNSUPPORTED;

    /* Optimize for no destination alpha (surface->pixman_image is non-NULL for all
     * surfaces with alpha.)
     */
    switch (categorize_solid_dest_operator (op, color->alpha_short)) {
    case DO_CLEAR:
        new_color.r = 0;
        new_color.g = 0;
        new_color.b = 0;
        break;
    case DO_SOURCE:
        new_color.r = color->red_short >> 8;
        new_color.g = color->green_short >> 8;
        new_color.b = color->blue_short >> 8;
        break;
    case DO_NOTHING:
        return CAIRO_STATUS_SUCCESS;
    case DO_UNSUPPORTED:
    default:
        return CAIRO_INT_STATUS_UNSUPPORTED;
    }

    SetBrushType (surface->dc, BT_SOLID);
    SetBrushColor (surface->dc, RGB2Pixel (surface->dc, new_color.r, new_color.g, new_color.b));
    for (i = 0; i < num_rects; i++) {
        FillBox (surface->dc, rects[i].x, rects[i].y, rects[i].width, rects[i].height);
    }

    return CAIRO_STATUS_SUCCESS;
}

static cairo_int_status_t
_cairo_minigui_surface_set_clip_region (void           *abstract_surface,
                                      cairo_region_t *region)
{
    cairo_minigui_surface_t *surface = abstract_surface;
    cairo_status_t status = CAIRO_STATUS_SUCCESS;

    /* If we are in-memory, then we set the clip on the image surface
     * as well as on the underlying GDI surface.
     */
    if (surface->image) {
        unsigned int serial;

        serial = _cairo_surface_allocate_clip_serial (surface->image);
        status = _cairo_surface_set_clip_region (surface->image, region, serial);
        if (status)
            return status;
    }

    /* The semantics we want is that any clip set by cairo combines
     * is intersected with the clip on device context that the
     * surface was created for. To implement this, we need to
     * save the original clip when first setting a clip on surface.
     */

    /* Clear any clip set by cairo, return to the original first */
    status = _cairo_minigui_restore_initial_clip (surface);

    /* Then combine any new region with it */
    if (region) {
        cairo_rectangle_int_t extents;
        cairo_box_int_t *boxes;
        int i, num_boxes;

        /* Create a GDI region for the cairo region */

        _cairo_region_get_extents (region, &extents);
        status = _cairo_region_get_boxes (region, &num_boxes, &boxes);
        if (status)
            return status;

        if (num_boxes == 1 && 
            boxes[0].p1.x == 0 &&
            boxes[0].p1.y == 0 &&
            boxes[0].p2.x == surface->extents.width &&
            boxes[0].p2.y == surface->extents.height)
        {
            RECT clip_rc = {boxes[0].p1.x, boxes[0].p1.y, boxes[0].p2.x, boxes[0].p2.y};

            /* don't use like this */
            /*SelectClipRegion (surface->dc, NULL);
            ClipRectIntersect (surface->dc, &clip_rc);*/
            SelectClipRect (surface->dc, &clip_rc);
        }
        else {
            CLIPRGN* cairo_cliprgn = CreateClipRgn ();
            CLIPRGN* curr_cliprgn = CreateClipRgn ();
            for (i = 0; i < num_boxes; i++) {
                RECT clip_rc = {boxes[i].p1.x, boxes[i].p1.y, boxes[i].p2.x, boxes[i].p2.y};
		 	 	AddClipRect (cairo_cliprgn, &clip_rc);
            }

            GetClipRegion (surface->dc, curr_cliprgn);
            ClipRgnIntersect (cairo_cliprgn, cairo_cliprgn, curr_cliprgn);
            SelectClipRegion (surface->dc, cairo_cliprgn);

            DestroyClipRgn(cairo_cliprgn);
            DestroyClipRgn(curr_cliprgn);
        }

        _cairo_region_boxes_fini (region, boxes);
    }
    /* when region is NULL, there means no use clip region,
     * it will draw no limit. */
    else {
        int w = GetGDCapability(surface->dc, GDCAP_HPIXEL);
        int h = GetGDCapability(surface->dc, GDCAP_VPIXEL);
        RECT clip_rc = {0, 0, w, h};

        SelectClipRect (surface->dc, &clip_rc);
    }

    return status;
}

static cairo_int_status_t
_cairo_minigui_surface_get_extents (void                          *abstract_surface,
                                  cairo_rectangle_int_t   *rectangle)
{
    cairo_minigui_surface_t *surface = abstract_surface;

    *rectangle = surface->extents;

    return CAIRO_STATUS_SUCCESS;
}

static cairo_status_t
_cairo_minigui_surface_flush (void *abstract_surface)
{
    return _cairo_surface_reset_clip (abstract_surface);
}

/**
 * cairo_minigui_surface_create:
 * @hdc: the DC to create a surface for
 *
 * Creates a cairo surface that targets the given DC.  The DC will be
 * queried for its initial clip extents, and this will be used as the
 * size of the cairo surface.  The resulting surface will always be of
 * format %CAIRO_FORMAT_RGB24; should you need another surface format,
 * you will need to create one through
 * cairo_minigui_surface_create_with_dib().
 *
 * Return value: the newly created surface
 **/
cairo_surface_t *
cairo_minigui_surface_create (HDC hdc)
{
    int bpp = 0;
    cairo_minigui_surface_t *surface;

    cairo_format_t format;
    RECT rect;

    /* according MiniGUI hdc format create cairo format */
    bpp = GetGDCapability(hdc, GDCAP_BITSPP);
    if (32 == bpp) {
        format = CAIRO_FORMAT_ARGB32;
    }
    else {
        format = CAIRO_FORMAT_RGB24;
    }

    surface = malloc (sizeof (cairo_minigui_surface_t));
    if (surface == NULL)
        return _cairo_surface_create_in_error (_cairo_error (CAIRO_STATUS_NO_MEMORY));

    if (_cairo_minigui_save_initial_clip (hdc, surface) != CAIRO_STATUS_SUCCESS) {
        free (surface);
        return _cairo_surface_create_in_error (_cairo_error (CAIRO_STATUS_NO_MEMORY));
    }

    surface->image = NULL;
    surface->format = format;

    surface->dc = hdc;
    surface->bitmap = NULL;
    surface->is_dib = FALSE;

    GetClipBox(hdc, &rect);
    surface->extents.x = rect.left;
    surface->extents.y = rect.top;
    surface->extents.width = rect.right - rect.left;
    surface->extents.height = rect.bottom - rect.top;

    surface->flags = _cairo_minigui_flags_for_dc (surface->dc);

    _cairo_surface_init (&surface->base, &cairo_minigui_surface_backend,
                         _cairo_content_from_format (format));

    return (cairo_surface_t *)surface;
}

/**
 * cairo_minigui_surface_create_with_dib:
 * @format: format of pixels in the surface to create
 * @width: width of the surface, in pixels
 * @height: height of the surface, in pixels
 *
 * Creates a device-independent-bitmap surface not associated with
 * any particular existing surface or device context. The created
 * bitmap will be uninitialized.
 *
 * Return value: the newly created surface
 *
 * Since: 1.2
 **/
cairo_surface_t *
cairo_minigui_surface_create_with_dib (cairo_format_t format,
                                     int            width,
                                     int            height)
{
    return _cairo_minigui_surface_create_for_dc (HDC_SCREEN, format, width, height);
}

/**
 * cairo_minigui_surface_create_with_ddb:
 * @hdc: the DC to create a surface for
 * @format: format of pixels in the surface to create
 * @width: width of the surface, in pixels
 * @height: height of the surface, in pixels
 *
 * Creates a device-independent-bitmap surface not associated with
 * any particular existing surface or device context. The created
 * bitmap will be uninitialized.
 *
 * Return value: the newly created surface
 *
 * Since: 1.4
 **/
cairo_surface_t *
cairo_minigui_surface_create_with_ddb (HDC hdc,
                                     cairo_format_t format,
                                     int width,
                                     int height)
{
    cairo_minigui_surface_t *new_surf;
    PBITMAP ddb;
    HDC ddb_dc;

    if (format != CAIRO_FORMAT_RGB24)
        return _cairo_surface_create_in_error (_cairo_error (CAIRO_STATUS_INVALID_FORMAT));

    if (!hdc) {
        hdc = HDC_SCREEN;
    }

    ddb_dc = CreateCompatibleDCEx (hdc, width, height);

    if (ddb_dc == HDC_INVALID) {
        new_surf = (cairo_minigui_surface_t*) _cairo_surface_create_in_error (_cairo_error (CAIRO_STATUS_NO_MEMORY));
        goto FINISH;
    }

    ddb = create_ddb_from_dc (hdc);
    if (ddb == NULL) {
        DeleteMemDC (ddb_dc);

        /* Note that if an app actually does hit this out of memory
         * condition, it's going to have lots of other issues, as
         * video memory is probably exhausted.  However, it can often
         * continue using DIBs instead of DDBs.
         */
        new_surf = (cairo_minigui_surface_t*) _cairo_surface_create_in_error (_cairo_error (CAIRO_STATUS_NO_MEMORY));
        goto FINISH;
    }

    new_surf = (cairo_minigui_surface_t*) cairo_minigui_surface_create (ddb_dc);
    new_surf->bitmap = ddb;
    new_surf->is_dib = FALSE;

FINISH:
    return (cairo_surface_t*) new_surf;
}

/**
 * _cairo_surface_is_minigui:
 * @surface: a #cairo_surface_t
 *
 * Checks if a surface is a minigui surface.  This will
 * return False if this is a minigui printing surface; use
 * _cairo_surface_is_minigui_printing() to check for that.
 *
 * Return value: True if the surface is an minigui surface
 **/
static int
_cairo_surface_is_minigui (cairo_surface_t *surface)
{
    return surface->backend == &cairo_minigui_surface_backend;
}

/**
 * cairo_minigui_surface_get_dc
 * @surface: a #cairo_surface_t
 *
 * Returns the HDC associated with this surface, or %NULL if none.
 * Also returns %NULL if the surface is not a minigui surface.
 *
 * Return value: HDC or %NULL if no HDC available.
 *
 * Since: 1.2
 **/
HDC
cairo_minigui_surface_get_dc (cairo_surface_t *surface)
{
    cairo_minigui_surface_t *mgsurf;

    if ( NULL == surface )
        return HDC_INVALID;

    if (_cairo_surface_is_minigui (surface)){
        mgsurf = (cairo_minigui_surface_t *) surface;

        return mgsurf->dc;
    }

    return HDC_INVALID;
}

/**
 * cairo_minigui_surface_get_image
 * @surface: a #cairo_surface_t
 *
 * Returns a #cairo_surface_t image surface that refers to the same bits
 * as the DIB of the MiniGUI surface.  If the passed-in minigui surface
 * is not a DIB surface, %NULL is returned.
 *
 * Return value: a #cairo_surface_t (owned by the minigui #cairo_surface_t),
 * or %NULL if the minigui surface is not a DIB.
 *
 * Since: 1.4
 */
cairo_surface_t *
cairo_minigui_surface_get_image (cairo_surface_t *surface)
{
    if (!_cairo_surface_is_minigui(surface))
        return NULL;

    return ((cairo_minigui_surface_t*)surface)->image;
}

static cairo_bool_t
_cairo_minigui_surface_is_similar (void *surface_a,
                                 void *surface_b,
                                 cairo_content_t content)
{
    cairo_minigui_surface_t *a = surface_a;
    cairo_minigui_surface_t *b = surface_b;

    return a->dc == b->dc;
}

static cairo_status_t
_cairo_minigui_surface_reset (void *abstract_surface)
{
    cairo_minigui_surface_t *surface = abstract_surface;
    cairo_status_t status;

    status = _cairo_minigui_surface_set_clip_region (surface, NULL);
    if (status)
        return status;

    return CAIRO_STATUS_SUCCESS;
}

static const cairo_surface_backend_t cairo_minigui_surface_backend = {
    CAIRO_SURFACE_TYPE_MINIGUI,
    _cairo_minigui_surface_create_similar,
    _cairo_minigui_surface_finish,
    _cairo_minigui_surface_acquire_source_image,
    _cairo_minigui_surface_release_source_image,
    _cairo_minigui_surface_acquire_dest_image,
    _cairo_minigui_surface_release_dest_image,
    _cairo_minigui_surface_clone_similar,
    _cairo_minigui_surface_composite,
    _cairo_minigui_surface_fill_rectangles,
    NULL, /* composite_trapezoids */
    NULL, /* copy_page */
    NULL, /* show_page */
    _cairo_minigui_surface_set_clip_region,
    NULL, /* intersect_clip_path */
    _cairo_minigui_surface_get_extents,
    NULL, /* old_show_glyphs */
    NULL, /* get_font_options */
    _cairo_minigui_surface_flush,
    NULL, /* mark_dirty_rectangle */
    NULL, /* scaled_font_fini */
    NULL, /* scaled_glyph_fini */

    NULL, /* paint */
    NULL, /* mask */
    NULL, /* stroke */
    NULL, /* fill */
    NULL, /* show_glyphs */
    NULL, /* snapshot */
    _cairo_minigui_surface_is_similar,

    _cairo_minigui_surface_reset
};

#if 0
static void
_cairo_minigui_debug_dump_hrgn (HRGN rgn, char *header)
{
    RGNDATA *rd;
    int z;

    if (header)
        fprintf (stderr, "%s\n", header);

    if (rgn == NULL) {
        fprintf (stderr, " NULL\n");
    }

    z = GetRegionData(rgn, 0, NULL);
    rd = (RGNDATA*) malloc(z);
    z = GetRegionData(rgn, z, rd);

    fprintf (stderr, " %d rects, bounds: %d %d %d %d\n", rd->rdh.nCount, rd->rdh.rcBound.left, rd->rdh.rcBound.top, rd->rdh.rcBound.right - rd->rdh.rcBound.left, rd->rdh.rcBound.bottom - rd->rdh.rcBound.top);

    for (z = 0; z < rd->rdh.nCount; z++) {
        RECT r = ((RECT*)rd->Buffer)[z];
        fprintf (stderr, " [%d]: [%d %d %d %d]\n", z, r.left, r.top, r.right - r.left, r.bottom - r.top);
    }

    free(rd);
    fflush (stderr);
}
#endif

