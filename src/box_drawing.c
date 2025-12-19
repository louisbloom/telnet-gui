/* Manual box drawing implementation */

#include "box_drawing.h"

int is_box_drawing_char(uint32_t codepoint) {
    return (codepoint >= 0x2500 && codepoint <= 0x257F);
}

void render_box_drawing_char(SDL_Renderer *renderer, uint32_t codepoint, int x, int y, int w, int h, SDL_Color color) {
    /* Set color */
    SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, color.a);

    /* Calculate geometry */
    /* Center point */
    int cx = x + w / 2;
    int cy = y + h / 2;

    /* Line thickness - adaptive but at least 1 pixel */
    /* Typical terminal font stroke is ~1/8th of em height? */
    /* Let's use standard 1px or 2px depending on cell size */
    int thickness = (h < 20) ? 1 : (h / 8);
    if (thickness < 1)
        thickness = 1;

    /* Half thickness for offsets */
    int ht = thickness / 2;

    /* Adjust center if thickness is even to ensure centering? */
    /* If thickness is odd (1), center is at a pixel. */
    /* If thickness is even (2), center spans two pixels. */
    /* Simple approach: draw centered on cx, cy */

    /* Define arms boolean flags */
    int up = 0;
    int down = 0;
    int left = 0;
    int right = 0;

    /* Determine which arms are active based on codepoint */
    switch (codepoint) {
    /* Horizontal: ─ */
    case 0x2500: /* LIGHT HORIZONTAL */
    case 0x2501: /* HEAVY HORIZONTAL */
    case 0x254C: /* LIGHT DOUBLE DASH HORIZONTAL */
    case 0x254D: /* HEAVY DOUBLE DASH HORIZONTAL */
    case 0x2550: /* DOUBLE HORIZONTAL */
        left = 1;
        right = 1;
        break;

    /* Vertical: │ */
    case 0x2502: /* LIGHT VERTICAL */
    case 0x2503: /* HEAVY VERTICAL */
    case 0x254E: /* LIGHT DOUBLE DASH VERTICAL */
    case 0x254F: /* HEAVY DOUBLE DASH VERTICAL */
    case 0x2551: /* DOUBLE VERTICAL */
        up = 1;
        down = 1;
        break;

    /* Corners */
    case 0x250C: /* LIGHT DOWN AND RIGHT (┌) */
    case 0x250D: /* DOWN LIGHT AND RIGHT HEAVY */
    case 0x250E: /* DOWN HEAVY AND RIGHT LIGHT */
    case 0x250F: /* HEAVY DOWN AND RIGHT */
    case 0x2552: /* DOWN SINGLE AND RIGHT DOUBLE */
    case 0x2553: /* DOWN DOUBLE AND RIGHT SINGLE */
    case 0x2554: /* DOUBLE DOWN AND RIGHT */
        down = 1;
        right = 1;
        break;

    case 0x2510: /* LIGHT DOWN AND LEFT (┐) */
    case 0x2511: /* DOWN LIGHT AND LEFT HEAVY */
    case 0x2512: /* DOWN HEAVY AND LEFT LIGHT */
    case 0x2513: /* HEAVY DOWN AND LEFT */
    case 0x2555: /* DOWN SINGLE AND LEFT DOUBLE */
    case 0x2556: /* DOWN DOUBLE AND LEFT SINGLE */
    case 0x2557: /* DOUBLE DOWN AND LEFT */
        down = 1;
        left = 1;
        break;

    case 0x2514: /* LIGHT UP AND RIGHT (└) */
    case 0x2515: /* UP LIGHT AND RIGHT HEAVY */
    case 0x2516: /* UP HEAVY AND RIGHT LIGHT */
    case 0x2517: /* HEAVY UP AND RIGHT */
    case 0x2558: /* UP SINGLE AND RIGHT DOUBLE */
    case 0x2559: /* UP DOUBLE AND RIGHT SINGLE */
    case 0x255A: /* DOUBLE UP AND RIGHT */
        up = 1;
        right = 1;
        break;

    case 0x2518: /* LIGHT UP AND LEFT (┘) */
    case 0x2519: /* UP LIGHT AND LEFT HEAVY */
    case 0x251A: /* UP HEAVY AND LEFT LIGHT */
    case 0x251B: /* HEAVY UP AND LEFT */
    case 0x255B: /* UP SINGLE AND LEFT DOUBLE */
    case 0x255C: /* UP DOUBLE AND LEFT SINGLE */
    case 0x255D: /* DOUBLE UP AND LEFT */
        up = 1;
        left = 1;
        break;

    /* Tees */
    case 0x251C: /* LIGHT VERTICAL AND RIGHT (├) */
    case 0x251D: /* VERTICAL LIGHT AND RIGHT HEAVY */
    case 0x251E: /* UP HEAVY AND RIGHT DOWN LIGHT */
    case 0x251F: /* DOWN HEAVY AND RIGHT UP LIGHT */
    case 0x2520: /* VERTICAL HEAVY AND RIGHT LIGHT */
    case 0x2521: /* DOWN LIGHT AND RIGHT UP HEAVY */
    case 0x2522: /* UP LIGHT AND RIGHT DOWN HEAVY */
    case 0x2523: /* HEAVY VERTICAL AND RIGHT */
    case 0x255E: /* VERTICAL SINGLE AND RIGHT DOUBLE */
    case 0x255F: /* VERTICAL DOUBLE AND RIGHT SINGLE */
    case 0x2560: /* DOUBLE VERTICAL AND RIGHT */
        up = 1;
        down = 1;
        right = 1;
        break;

    case 0x2524: /* LIGHT VERTICAL AND LEFT (┤) */
    case 0x2525: /* VERTICAL LIGHT AND LEFT HEAVY */
    case 0x2526: /* UP HEAVY AND LEFT DOWN LIGHT */
    case 0x2527: /* DOWN HEAVY AND LEFT UP LIGHT */
    case 0x2528: /* VERTICAL HEAVY AND LEFT LIGHT */
    case 0x2529: /* DOWN LIGHT AND LEFT UP HEAVY */
    case 0x252A: /* UP LIGHT AND LEFT DOWN HEAVY */
    case 0x252B: /* HEAVY VERTICAL AND LEFT */
    case 0x2561: /* VERTICAL SINGLE AND LEFT DOUBLE */
    case 0x2562: /* VERTICAL DOUBLE AND LEFT SINGLE */
    case 0x2563: /* DOUBLE VERTICAL AND LEFT */
        up = 1;
        down = 1;
        left = 1;
        break;

    case 0x252C: /* LIGHT DOWN AND HORIZONTAL (┬) */
    case 0x252D: /* LEFT HEAVY AND RIGHT DOWN LIGHT */
    case 0x252E: /* RIGHT HEAVY AND LEFT DOWN LIGHT */
    case 0x252F: /* DOWN LIGHT AND HORIZONTAL HEAVY */
    case 0x2530: /* DOWN HEAVY AND HORIZONTAL LIGHT */
    case 0x2531: /* RIGHT LIGHT AND LEFT DOWN HEAVY */
    case 0x2532: /* LEFT LIGHT AND RIGHT DOWN HEAVY */
    case 0x2533: /* HEAVY DOWN AND HORIZONTAL */
    case 0x2564: /* DOWN SINGLE AND HORIZONTAL DOUBLE */
    case 0x2565: /* DOWN DOUBLE AND HORIZONTAL SINGLE */
    case 0x2566: /* DOUBLE DOWN AND HORIZONTAL */
        down = 1;
        left = 1;
        right = 1;
        break;

    case 0x2534: /* LIGHT UP AND HORIZONTAL (┴) */
    case 0x2535: /* LEFT HEAVY AND RIGHT UP LIGHT */
    case 0x2536: /* RIGHT HEAVY AND LEFT UP LIGHT */
    case 0x2537: /* UP LIGHT AND HORIZONTAL HEAVY */
    case 0x2538: /* UP HEAVY AND HORIZONTAL LIGHT */
    case 0x2539: /* RIGHT LIGHT AND LEFT UP HEAVY */
    case 0x253A: /* LEFT LIGHT AND RIGHT UP HEAVY */
    case 0x253B: /* HEAVY UP AND HORIZONTAL */
    case 0x2567: /* UP SINGLE AND HORIZONTAL DOUBLE */
    case 0x2568: /* UP DOUBLE AND HORIZONTAL SINGLE */
    case 0x2569: /* DOUBLE UP AND HORIZONTAL */
        up = 1;
        left = 1;
        right = 1;
        break;

    /* Cross */
    case 0x253C: /* LIGHT VERTICAL AND HORIZONTAL (┼) */
    case 0x253D: /* LEFT HEAVY AND RIGHT VERTICAL LIGHT */
    case 0x253E: /* RIGHT HEAVY AND LEFT VERTICAL LIGHT */
    case 0x253F: /* VERTICAL LIGHT AND HORIZONTAL HEAVY */
    case 0x2540: /* UP HEAVY AND DOWN HORIZONTAL LIGHT */
    case 0x2541: /* DOWN HEAVY AND UP HORIZONTAL LIGHT */
    case 0x2542: /* VERTICAL HEAVY AND HORIZONTAL LIGHT */
    case 0x2543: /* LEFT UP HEAVY AND RIGHT DOWN LIGHT */
    case 0x2544: /* RIGHT UP HEAVY AND LEFT DOWN LIGHT */
    case 0x2545: /* LEFT DOWN HEAVY AND RIGHT UP LIGHT */
    case 0x2546: /* RIGHT DOWN HEAVY AND LEFT UP LIGHT */
    case 0x2547: /* DOWN LIGHT AND UP HORIZONTAL HEAVY */
    case 0x2548: /* UP LIGHT AND DOWN HORIZONTAL HEAVY */
    case 0x2549: /* RIGHT LIGHT AND LEFT VERTICAL HEAVY */
    case 0x254A: /* LEFT LIGHT AND RIGHT VERTICAL HEAVY */
    case 0x254B: /* HEAVY VERTICAL AND HORIZONTAL */
    case 0x256A: /* VERTICAL SINGLE AND HORIZONTAL DOUBLE */
    case 0x256B: /* VERTICAL DOUBLE AND HORIZONTAL SINGLE */
    case 0x256C: /* DOUBLE VERTICAL AND HORIZONTAL */
        up = 1;
        down = 1;
        left = 1;
        right = 1;
        break;

    default:
        /* For unknown chars in the range, do nothing or fallback to font (caller should handle) */
        /* But caller checks is_box_drawing_char first. If we are here, we should draw something. */
        /* If we don't know it, draw a '?' box? Or just nothing. */
        return;
    }

    /* Draw active arms */
    /* Center intersection */
    SDL_Rect center_rect = {cx - ht, cy - ht, thickness, thickness};
    SDL_RenderFillRect(renderer, &center_rect);

    if (up) {
        SDL_Rect r = {cx - ht, y, thickness, (cy - ht) - y + 1}; /* Extend to center */
        SDL_RenderFillRect(renderer, &r);
    }
    if (down) {
        SDL_Rect r = {cx - ht, cy + ht, thickness, (y + h) - (cy + ht)};
        SDL_RenderFillRect(renderer, &r);
    }
    if (left) {
        SDL_Rect r = {x, cy - ht, (cx - ht) - x + 1, thickness}; /* Extend to center */
        SDL_RenderFillRect(renderer, &r);
    }
    if (right) {
        SDL_Rect r = {cx + ht, cy - ht, (x + w) - (cx + ht), thickness};
        SDL_RenderFillRect(renderer, &r);
    }
}
