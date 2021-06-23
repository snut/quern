
# .hdr loading (panorama environment maps)
compute work:
- bc6 compression
- mip chain generation /w preconvolution
codec:
- investigate saving to cubemap with mips and block compression
- custom vs dds

# .png/.jpg/etc to RGB(A) 2D
compute work:
- mip chain generation
- bc7 compression

BC6 impl:
https://github.com/knarkowicz/GPURealTimeBC6H/blob/master/bin/compress.hlsl

# animation system
- transform interpolation /w begin and end
- looping (idle) and one-shot
- timed events (vfx, sfx, text)

# vfx system
- ribbons
- blobby particles (slime, water)
- environmental effects (leaves, dust, pebbles, mist)
- design/exploration for magical things? big billboards or volumetrics or..?

# rendering
- animated foliage
- ssao
- post effects (bloom, motion blur?, radial blur?)

# gui
- aaaaah


# random notes:

https://flight-manual.atom.io/using-atom/sections/basic-customization/#customizing-language-recognition

customFileTypes:
    'source.cpp': ['comp','vert','frag','tesc','tese','geom']

windows powershell redirection with encoding (also -Append)
> stack build 2>&1 | Out-File build-log.txt -Encoding UTF8

`chcp 65001`

:set -fno-ghci-sandbox

# Substance-like texture generators
Materialize (http://www.boundingboxsoftware.com/materialize/getkey.php)

Imogen (https://github.com/CedricGuillemet/Imogen)

TexGraph (https://galloscript.itch.io/texgraph) [not OSS]
Material Maker (https://rodzilla.itch.io/material-maker/devlog/120610/material-maker-08)

Texture Lab (https://njbrown.itch.io/texturelab)
