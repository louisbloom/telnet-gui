# TODO

Future enhancements and improvements.

## Font Rendering

- [ ] **True bold font support**: Load separate bold font files (e.g., `DejaVuSansMono-Bold.ttf`) and use them for ANSI bold rendering instead of SDL_ttf's synthetic bold (`TTF_STYLE_BOLD`). Would require loading multiple font handles and swapping between them based on bold attribute. True bold fonts have properly designed letterforms and spacing compared to algorithmic thickening.
