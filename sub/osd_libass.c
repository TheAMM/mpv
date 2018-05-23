/*
 * This file is part of mpv.
 *
 * mpv is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * mpv is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with mpv.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <libavutil/common.h>

#include "config.h"

#include "mpv_talloc.h"
#include "misc/bstr.h"
#include "common/common.h"
#include "common/msg.h"
#include "osd.h"
#include "osd_state.h"

static const char osd_font_pfb[] =
#include "sub/osd_font.h"
;

#include "sub/ass_mp.h"
#include "options/options.h"


#define ASS_USE_OSD_FONT "{\\fnmpv-osd-symbols}"

void osd_init_backend(struct osd_state *osd)
{
}

static void create_ass_renderer(struct osd_state *osd, struct ass_state *ass)
{
    if (ass->render)
        return;

    ass->log = mp_log_new(NULL, osd->log, "libass");
    ass->library = mp_ass_init(osd->global, ass->log);
    ass_add_font(ass->library, "mpv-osd-symbols", (void *)osd_font_pfb,
                 sizeof(osd_font_pfb) - 1);

    ass->render = ass_renderer_init(ass->library);
    if (!ass->render)
        abort();

    mp_ass_configure_fonts(ass->render, osd->opts->osd_style,
                           osd->global, ass->log);
    ass_set_aspect_ratio(ass->render, 1.0, 1.0);
}

static void destroy_ass_renderer(struct ass_state *ass)
{
    if (ass->track)
        ass_free_track(ass->track);
    ass->track = NULL;
    if (ass->render)
        ass_renderer_done(ass->render);
    ass->render = NULL;
    if (ass->library)
        ass_library_done(ass->library);
    ass->library = NULL;
    talloc_free(ass->log);
    ass->log = NULL;
}

static void destroy_external(struct osd_external *ext)
{
    talloc_free(ext->text);
    destroy_ass_renderer(&ext->ass);
}

void osd_destroy_backend(struct osd_state *osd)
{
    for (int n = 0; n < MAX_OSD_PARTS; n++) {
        struct osd_object *obj = osd->objs[n];
        destroy_ass_renderer(&obj->ass);
        for (int i = 0; i < obj->num_externals; i++)
            destroy_external(&obj->externals[i]);
        obj->num_externals = 0;
    }
}

static void update_playres(struct ass_state *ass, struct mp_osd_res *vo_res)
{
    ASS_Track *track = ass->track;
    int old_res_x = track->PlayResX;
    int old_res_y = track->PlayResY;

    double aspect = 1.0 * vo_res->w / MPMAX(vo_res->h, 1);
    if (vo_res->display_par > 0)
        aspect = aspect / vo_res->display_par;

    track->PlayResY = ass->res_y ? ass->res_y : MP_ASS_FONT_PLAYRESY;
    track->PlayResX = ass->res_x ? ass->res_x : track->PlayResY * aspect;

    // Force libass to clear its internal cache - it doesn't check for
    // PlayRes changes itself.
    if (old_res_x != track->PlayResX || old_res_y != track->PlayResY)
        ass_set_frame_size(ass->render, 1, 1);
}

static void create_ass_track(struct osd_state *osd, struct osd_object *obj,
                             struct ass_state *ass)
{
    create_ass_renderer(osd, ass);

    ASS_Track *track = ass->track;
    if (!track)
        track = ass->track = ass_new_track(ass->library);

    track->track_type = TRACK_TYPE_ASS;
    track->Timer = 100.;
    track->WrapStyle = 1; // end-of-line wrapping instead of smart wrapping
    track->Kerning = true;

    update_playres(ass, &obj->vo_res);
}

static int find_style(ASS_Track *track, const char *name, int def)
{
    for (int n = 0; n < track->n_styles; n++) {
        if (track->styles[n].Name && strcmp(track->styles[n].Name, name) == 0)
            return n;
    }
    return def;
}

// Find a given style, or add it if it's missing.
static ASS_Style *get_style(struct ass_state *ass, char *name)
{
    ASS_Track *track = ass->track;
    if (!track)
        return NULL;

    int sid = find_style(track, name, -1);
    if (sid >= 0)
        return &track->styles[sid];

    sid = ass_alloc_style(track);
    ASS_Style *style = &track->styles[sid];
    style->Name = strdup(name);
    // Set to neutral base direction, as opposed to VSFilter LTR default
    style->Encoding = -1;
    return style;
}

static ASS_Event *add_osd_ass_event(ASS_Track *track, const char *style,
                                    const char *text)
{
    int n = ass_alloc_event(track);
    ASS_Event *event = track->events + n;
    event->Start = 0;
    event->Duration = 100;
    event->Style = find_style(track, style, 0);
    event->ReadOrder = n;
    assert(event->Text == NULL);
    if (text)
        event->Text = strdup(text);
    return event;
}

static void clear_ass(struct ass_state *ass)
{
    if (ass->track)
        ass_flush_events(ass->track);
}

void osd_get_function_sym(char *buffer, size_t buffer_size, int osd_function)
{
    // 0xFF is never valid UTF-8, so we can use it to escape OSD symbols.
    // (Same trick as OSD_ASS_0/OSD_ASS_1.)
    snprintf(buffer, buffer_size, "\xFF%c", osd_function);
}

static void mangle_ass(bstr *dst, const char *in)
{
    const char *start = in;
    bool escape_ass = true;
    while (*in) {
        // As used by osd_get_function_sym().
        if (in[0] == '\xFF' && in[1]) {
            bstr_xappend(NULL, dst, bstr0(ASS_USE_OSD_FONT));
            mp_append_utf8_bstr(NULL, dst, OSD_CODEPOINTS + in[1]);
            bstr_xappend(NULL, dst, bstr0("{\\r}"));
            in += 2;
            continue;
        }
        if (*in == OSD_ASS_0[0] || *in == OSD_ASS_1[0]) {
            escape_ass = *in == OSD_ASS_1[0];
            in += 1;
            continue;
        }
        if (escape_ass && *in == '{')
            bstr_xappend(NULL, dst, bstr0("\\"));
        // Libass will strip leading whitespace
        if (in[0] == ' ' && (in == start || in[-1] == '\n')) {
            bstr_xappend(NULL, dst, bstr0("\\h"));
            in += 1;
            continue;
        }
        bstr_xappend(NULL, dst, (bstr){(char *)in, 1});
        // Break ASS escapes with U+2060 WORD JOINER
        if (escape_ass && *in == '\\')
            mp_append_utf8_bstr(NULL, dst, 0x2060);
        in++;
    }
}

static ASS_Event *add_osd_ass_event_escaped(ASS_Track *track, const char *style,
                                            const char *text)
{
    bstr buf = {0};
    mangle_ass(&buf, text);
    ASS_Event *e = add_osd_ass_event(track, style, buf.start);
    talloc_free(buf.start);
    return e;
}

static ASS_Style *prepare_osd_ass(struct osd_state *osd, struct osd_object *obj)
{
    struct mp_osd_render_opts *opts = osd->opts;

    create_ass_track(osd, obj, &obj->ass);

    struct osd_style_opts font = *opts->osd_style;
    font.font_size *= opts->osd_scale;

    double playresy = obj->ass.track->PlayResY;
    // Compensate for libass and mp_ass_set_style scaling the font etc.
    if (!opts->osd_scale_by_window)
        playresy *= 720.0 / obj->vo_res.h;

    ASS_Style *style = get_style(&obj->ass, "OSD");
    mp_ass_set_style(style, playresy, &font);
    return style;
}

static void update_osd_text(struct osd_state *osd, struct osd_object *obj)
{

    if (!obj->text[0])
        return;

    prepare_osd_ass(osd, obj);
    add_osd_ass_event_escaped(obj->ass.track, "OSD", obj->text);
}

void osd_get_text_size(struct osd_state *osd, int *out_screen_h, int *out_font_h)
{
    pthread_mutex_lock(&osd->lock);
    struct osd_object *obj = osd->objs[OSDTYPE_OSD];
    ASS_Style *style = prepare_osd_ass(osd, obj);
    *out_screen_h = obj->ass.track->PlayResY - style->MarginV;
    *out_font_h = style->FontSize;
    pthread_mutex_unlock(&osd->lock);
}

// align: -1 .. +1
// frame: size of the containing area
// obj: size of the object that should be positioned inside the area
// margin: min. distance from object to frame (as long as -1 <= align <= +1)
static float get_align(float align, float frame, float obj, float margin)
{
    frame -= margin * 2;
    return margin + frame / 2 - obj / 2 + (frame - obj) / 2 * align;
}

struct ass_draw {
    int scale;
    char *text;
};

static void ass_draw_start(struct ass_draw *d)
{
    d->scale = FFMAX(d->scale, 1);
    d->text = talloc_asprintf_append(d->text, "{\\p%d}", d->scale);
}

static void ass_draw_stop(struct ass_draw *d)
{
    d->text = talloc_strdup_append(d->text, "{\\p0}");
}

static void ass_draw_c(struct ass_draw *d, float x, float y)
{
    int ix = round(x * (1 << (d->scale - 1)));
    int iy = round(y * (1 << (d->scale - 1)));
    d->text = talloc_asprintf_append(d->text, " %d %d", ix, iy);
}

static void ass_draw_append(struct ass_draw *d, const char *t)
{
    d->text = talloc_strdup_append(d->text, t);
}

static void ass_draw_move_to(struct ass_draw *d, float x, float y)
{
    ass_draw_append(d, " m");
    ass_draw_c(d, x, y);
}

static void ass_draw_line_to(struct ass_draw *d, float x, float y)
{
    ass_draw_append(d, " l");
    ass_draw_c(d, x, y);
}

static void ass_draw_rect_ccw(struct ass_draw *d, float x0, float y0,
                              float x1, float y1)
{
    ass_draw_move_to(d, x0, y0);
    ass_draw_line_to(d, x0, y1);
    ass_draw_line_to(d, x1, y1);
    ass_draw_line_to(d, x1, y0);
}

static void ass_draw_rect_cw(struct ass_draw *d, float x0, float y0,
                             float x1, float y1)
{
    ass_draw_move_to(d, x0, y0);
    ass_draw_line_to(d, x1, y0);
    ass_draw_line_to(d, x1, y1);
    ass_draw_line_to(d, x0, y1);
}

static void ass_draw_reset(struct ass_draw *d)
{
    talloc_free(d->text);
    d->text = NULL;
}

static void get_osd_bar_box(struct osd_state *osd, struct osd_object *obj,
                            float *o_x, float *o_y, float *o_w, float *o_h,
                            float *o_border)
{
    struct mp_osd_render_opts *opts = osd->opts;

    create_ass_track(osd, obj, &obj->ass);
    ASS_Track *track = obj->ass.track;

    ASS_Style *style = get_style(&obj->ass, "progbar");
    if (!style) {
        *o_x = *o_y = *o_w = *o_h = *o_border = 0;
        return;
    }

    mp_ass_set_style(style, track->PlayResY, opts->osd_style);

    *o_w = track->PlayResX * (opts->osd_bar_w / 100.0);
    *o_h = track->PlayResY * (opts->osd_bar_h / 100.0);

    float base_size = 0.03125;
    style->Outline *= *o_h / track->PlayResY / base_size;
    // So that the chapter marks have space between them
    style->Outline = FFMIN(style->Outline, *o_h / 5.2);
    // So that the border is not 0
    style->Outline = FFMAX(style->Outline, *o_h / 32.0);
    // Rendering with shadow is broken (because there's more than one shape)
    style->Shadow = 0;

    style->Alignment = 5;

    *o_border = style->Outline;

    *o_x = get_align(opts->osd_bar_align_x, track->PlayResX, *o_w, *o_border);
    *o_y = get_align(opts->osd_bar_align_y, track->PlayResY, *o_h, *o_border);
}

static void update_progbar(struct osd_state *osd, struct osd_object *obj)
{
    if (obj->progbar_state.type < 0)
        return;

    float px, py, width, height, border;
    get_osd_bar_box(osd, obj, &px, &py, &width, &height, &border);

    ASS_Track *track = obj->ass.track;

    float sx = px - border * 2 - height / 4; // includes additional spacing
    float sy = py + height / 2;

    bstr buf = bstr0(talloc_asprintf(NULL, "{\\an6\\pos(%f,%f)}", sx, sy));

    if (obj->progbar_state.type == 0 || obj->progbar_state.type >= 256) {
        // no sym
    } else if (obj->progbar_state.type >= 32) {
        mp_append_utf8_bstr(NULL, &buf, obj->progbar_state.type);
    } else {
        bstr_xappend(NULL, &buf, bstr0(ASS_USE_OSD_FONT));
        mp_append_utf8_bstr(NULL, &buf, OSD_CODEPOINTS + obj->progbar_state.type);
        bstr_xappend(NULL, &buf, bstr0("{\\r}"));
    }

    add_osd_ass_event(track, "progbar", buf.start);
    talloc_free(buf.start);

    struct ass_draw *d = &(struct ass_draw) { .scale = 4 };
    // filled area
    d->text = talloc_asprintf_append(d->text, "{\\bord0\\pos(%f,%f)}", px, py);
    ass_draw_start(d);
    float pos = obj->progbar_state.value * width - border / 2;
    ass_draw_rect_cw(d, 0, 0, pos, height);
    ass_draw_stop(d);
    add_osd_ass_event(track, "progbar", d->text);
    ass_draw_reset(d);

    // position marker
    d->text = talloc_asprintf_append(d->text, "{\\bord%f\\pos(%f,%f)}",
                                     border / 2, px, py);
    ass_draw_start(d);
    ass_draw_move_to(d, pos + border / 2, 0);
    ass_draw_line_to(d, pos + border / 2, height);
    ass_draw_stop(d);
    add_osd_ass_event(track, "progbar", d->text);
    ass_draw_reset(d);

    d->text = talloc_asprintf_append(d->text, "{\\pos(%f,%f)}", px, py);
    ass_draw_start(d);

    // the box
    ass_draw_rect_cw(d, -border, -border, width + border, height + border);

    // the "hole"
    ass_draw_rect_ccw(d, 0, 0, width, height);

    // chapter marks
    for (int n = 0; n < obj->progbar_state.num_stops; n++) {
        float s = obj->progbar_state.stops[n] * width;
        float dent = border * 1.3;

        if (s > dent && s < width - dent) {
            ass_draw_move_to(d, s + dent, 0);
            ass_draw_line_to(d, s,        dent);
            ass_draw_line_to(d, s - dent, 0);

            ass_draw_move_to(d, s - dent, height);
            ass_draw_line_to(d, s,        height - dent);
            ass_draw_line_to(d, s + dent, height);
        }
    }

    ass_draw_stop(d);
    add_osd_ass_event(track, "progbar", d->text);
    ass_draw_reset(d);
}

static void update_osd(struct osd_state *osd, struct osd_object *obj)
{
    obj->osd_changed = false;
    clear_ass(&obj->ass);
    update_osd_text(osd, obj);
    update_progbar(osd, obj);
}

static void update_external(struct osd_state *osd, struct osd_object *obj,
                            struct osd_external *ext)
{
    bstr t = bstr0(ext->text);
    if (!t.len)
        return;
    ext->ass.res_x = ext->res_x;
    ext->ass.res_y = ext->res_y;
    create_ass_track(osd, obj, &ext->ass);

    clear_ass(&ext->ass);

    int resy = ext->ass.track->PlayResY;
    mp_ass_set_style(get_style(&ext->ass, "OSD"), resy, osd->opts->osd_style);

    // Some scripts will reference this style name with \r tags.
    const struct osd_style_opts *def = osd_style_conf.defaults;
    mp_ass_set_style(get_style(&ext->ass, "Default"), resy, def);

    while (t.len) {
        bstr line;
        bstr_split_tok(t, "\n", &line, &t);
        if (line.len) {
            char *tmp = bstrdup0(NULL, line);
            add_osd_ass_event(ext->ass.track, "OSD", tmp);
            talloc_free(tmp);
        }
    }
}

void osd_set_external(struct osd_state *osd, void *id, int res_x, int res_y,
                      char *text)
{
    pthread_mutex_lock(&osd->lock);
    struct osd_object *obj = osd->objs[OSDTYPE_EXTERNAL];
    struct osd_external *entry = 0;
    for (int n = 0; n < obj->num_externals; n++) {
        if (obj->externals[n].id == id) {
            entry = &obj->externals[n];
            break;
        }
    }
    if (!entry && !text)
        goto done;

    if (!entry) {
        struct osd_external new = { .id = id };
        MP_TARRAY_APPEND(obj, obj->externals, obj->num_externals, new);
        entry = &obj->externals[obj->num_externals - 1];
    }

    if (!text) {
        int index = entry - &obj->externals[0];
        destroy_external(entry);
        MP_TARRAY_REMOVE_AT(obj->externals, obj->num_externals, index);
        obj->changed = true;
        osd->want_redraw_notification = true;
        goto done;
    }

    if (!entry->text || strcmp(entry->text, text) != 0 ||
        entry->res_x != res_x || entry->res_y != res_y)
    {
        talloc_free(entry->text);
        entry->text = talloc_strdup(NULL, text);
        entry->res_x = res_x;
        entry->res_y = res_y;
        update_external(osd, obj, entry);
        obj->changed = true;
        osd->want_redraw_notification = true;
    }

done:
    pthread_mutex_unlock(&osd->lock);
}

static void append_ass(struct ass_state *ass, struct mp_osd_res *res,
                       ASS_Image **img_list, bool *changed)
{
    if (!ass->render || !ass->track) {
        *img_list = NULL;
        return;
    }

    update_playres(ass, res);

    ass_set_frame_size(ass->render, res->w, res->h);
    ass_set_aspect_ratio(ass->render, res->display_par, 1.0);

    int ass_changed;
    *img_list = ass_render_frame(ass->render, ass->track, 0, &ass_changed);
    *changed |= ass_changed;
}

void osd_object_get_bitmaps(struct osd_state *osd, struct osd_object *obj,
                            int format, struct sub_bitmaps *out_imgs)
{
    if (obj->type == OSDTYPE_OSD && obj->osd_changed)
        update_osd(osd, obj);

    if (!obj->ass_packer)
        obj->ass_packer = mp_ass_packer_alloc(obj);

    MP_TARRAY_GROW(obj, obj->ass_imgs, obj->num_externals + 1);

    append_ass(&obj->ass, &obj->vo_res, &obj->ass_imgs[0], &obj->changed);
    for (int n = 0; n < obj->num_externals; n++) {
        append_ass(&obj->externals[n].ass, &obj->vo_res, &obj->ass_imgs[n + 1],
                   &obj->changed);
    }

    mp_ass_packer_pack(obj->ass_packer, obj->ass_imgs, obj->num_externals + 1,
                       obj->changed, format, out_imgs);

    obj->changed = false;
}


/* Finds a bounding box for a sub_bitmap by efficiently scanning the pixels
   from different sides. Assumes format is SUBBITMAP_LIBASS (A8). */
static void get_sub_bitmap_bounds(struct osd_state *osd, struct sub_bitmap *part,
                                unsigned char alpha_treshold,
                                struct mp_extents *extents,
                                bool *extents_set)
{
    if (*extents_set && (part->x >= extents->x1 &&
                        part->x + part->w <= extents->x2 &&
                        part->y >= extents->y1 &&
                        part->y + part->h <= extents->y2))
    {
        // Bail if the sub_bitmap is already contained within the current extents
        // MP_ERR(osd, "Bail\n");
        return;
    }

    int pw = part->w;
    int ph = part->h;

    int min_y = ph;
    int min_x = pw;
    int max_x = 0;
    int max_y = 0;

    bool is_blank = true;

    unsigned char* bitmap_data = (unsigned char*) part->bitmap;

    // Top to bottom +Y +X
    for (int y = 0; y < min_y; y++) {
        for (int x = 0; x < min_x; x++) {
            if (bitmap_data[part->stride * y + x] > alpha_treshold) {
                min_y = y;
                // Preliminary values to save loops later on
                max_y = y;
                min_x = x;
                max_x = x;

                is_blank = false;
            }
        }
    }

    if (is_blank) {
        // Bail early if we have an empty sub_bitmap
        return;
    }

    // Right to left -X -Y
    for (int x = pw-1; x > max_x; x--) {
        for (int y = ph-1; y > max_y; y--) {
            if (bitmap_data[part->stride * y + x] > alpha_treshold) {
                max_x = x;
                // Preliminary
                max_y = y;
                break;
            }
        }
    }

    // Bottom to top -Y -X
    for (int y = ph-1; y > max_y; y--) {
        for (int x = max_x; x > 0; x--) {
            if (bitmap_data[part->stride * y + x] > alpha_treshold) {
                max_y = y;
                // Preliminary
                min_x = x;
                break;
            }
        }
    }

    // Left to right +X +Y
    for (int x = 0; x < min_x; x++) {
        for (int y = min_y; y < max_y; y++) {
            if (bitmap_data[part->stride * y + x] > alpha_treshold) {
                min_x = x;
                break;
            }
        }
    }

    if (*extents_set) {
        // MP_INFO(osd, "  Is set\n");
        // Extend the existing extents
        extents->x1 = MPMIN(extents->x1, min_x + part->x);
        extents->x2 = MPMAX(extents->x2, max_x + part->x);
        extents->y1 = MPMIN(extents->y1, min_y + part->y);
        extents->y2 = MPMAX(extents->y2, max_y + part->y);
    } else {
        // MP_INFO(osd, "  Not set\n");
        // Set the fresh extents
        extents->x1 = min_x + part->x;
        extents->x2 = max_x + part->x;
        extents->y1 = min_y + part->y;
        extents->y2 = max_y + part->y;
        *extents_set = true;
    }
}

/* Calculates extents for given ASS subtitles as if they were displayed on the OSD.
   Subtitles are rendered with libass and fitted with a bounding box.
   This is relatively fast (5 to 10ms on an old laptop) but it's not advisable to
   calculate the extents every tick. */
bool osd_get_ass_extents(struct osd_state *osd, void *id, int res_x, int res_y,
                         char *text, unsigned char alpha_treshold, struct mp_extents *out_extents)
{
    struct mp_osd_res vo_res = osd_get_vo_res(osd);
    pthread_mutex_lock(&osd->lock);
    id = 1;
    // MP_ERR(osd, "Got lock %p %p, id: %p\n", osd, out_extents, id);

    // struct mp_extents *ass_extents;
    bool extents_set = false;

    struct osd_object *obj = osd->objs[OSDTYPE_EXTENTS];
    struct osd_external *entry = 0;
    for (int n = 0; n < obj->num_externals; n++) {
        if (obj->externals[n].id == id) {
            entry = &obj->externals[n];
            break;
        }
    }

    if (!entry && !text) {
        goto done;
    }

    if (!entry) {
        // Create new entry
        struct osd_external new = { .id = id };
        MP_TARRAY_APPEND(obj, obj->externals, obj->num_externals, new);
        entry = &obj->externals[obj->num_externals - 1];
    }

    if (!text) {
        // Remove entry
        int index = entry - &obj->externals[0];
        destroy_external(entry);
        MP_TARRAY_REMOVE_AT(obj->externals, obj->num_externals, index);
        goto done;
    }

    // Check if text or stage resolution has changed
    if (!entry->text || strcmp(entry->text, text) != 0 ||
        entry->res_x != res_x || entry->res_y != res_y)
    {
        // MP_WARN(osd, " pre-txt %.64s\n", entry->text);
        talloc_free(entry->text);
        entry->text = talloc_strdup(NULL, text);
        entry->res_x = res_x;
        entry->res_y = res_y;
        update_external(osd, obj, entry);
        // obj->changed = true;

        // ASS_Event *as_ev; = entry->ass.track->events;
        // ASS_Event *as_ev;
        // MP_WARN(osd, "Ass events:\n");
        // for (int i = 0; i < entry->ass.track->n_events; ++i)
        // {
        //     as_ev = entry->ass.track->events + i;
        //     MP_WARN(osd, " %02d Text: %.64s\n", i, as_ev->Text);
        // }

        void *old_ptr = obj->extents;
        talloc_free(obj->extents);
        // obj->extents = talloc_zero(NULL, struct mp_extents);
        // We don't need to zero because of extents_set
        obj->extents = talloc(NULL, struct mp_extents);
        // MP_WARN(osd, "Free and alloc %p -> %p\n", old_ptr, obj->extents);
        // MP_WARN(osd, " txt %.64s\n", entry->text);
        // MP_WARN(osd, " pre-extents %d,%d, %d,%d\n", obj->extents->x1, obj->extents->y1, obj->extents->x2, obj->extents->y2);

        if (!osd_res_equals(vo_res, obj->vo_res)) {
            obj->vo_res = vo_res;
        }
        struct sub_bitmaps imgs = {0};
        osd_object_get_bitmaps(osd, obj, SUBBITMAP_LIBASS, &imgs);

        // Check bounds for all sub-images
        for (int img_i = 0; img_i < imgs.num_parts; img_i++) {
            get_sub_bitmap_bounds(osd, &imgs.parts[img_i], alpha_treshold, obj->extents, &extents_set);
            // MP_WARN(osd, "  extents %d,%d, %d,%d\n", obj->extents->x1, obj->extents->y1, obj->extents->x2, obj->extents->y2);
        }
        // MP_WARN(osd, " extents_set %d parts %d\n", obj->extents_set, imgs.num_parts);
        // MP_WARN(osd, " extents %d,%d, %d,%d\n", obj->extents->x1, obj->extents->y1, obj->extents->x2, obj->extents->y2);

        // Store extents
        // obj->extents = ass_extents;
        obj->extents_set = extents_set;
    } else {
        // If ASS is cached, grab the done-state
        extents_set = obj->extents_set;
        // MP_WARN(osd, " cached set %d\n", obj->extents_set);
    }

    // Set output extents
    if (extents_set) {
        memcpy(out_extents, obj->extents, sizeof(struct mp_extents));
    }
    // *out_extents = obj->extents;

done:
    // MP_ERR(osd, "Leaving lock %p %p\n", osd, out_extents);
    pthread_mutex_unlock(&osd->lock);
    return extents_set;
}
