
- Sparks (the GPU particle effect)
- Blobs (liquid, slime)
- Ribbons (trails, lasers, lightning)

generating good flipbooks seems hard, de-emphasise large animated particles

blobs: sdf in a grid?



-- volumetric stuff?


-- transparency sorting

render transparent meshes to low-res billboards (imposters) in an atlas
- surface lighting
- translucent extinction/thickness
- distortion map
- depth/z

- keep details in a buffer:
- uv scale/bias for atlas
- clip space position (from bounds)
- min/max depth value (from bounds)

re-render transparent meshes
- full res surface lighting
- iterate through billboards behind pixel
- accumulate density, distortion

particles ideally sliced between billboards

--

completely compute-driven approach?
shove particles into clusters, sort them, write to image[s]?

1. kernel assigns particles to clusters
2. each cluster sorts particle array
3. cluster reads from composite image(s) to LDS (?), does rendering stuffs, writes back to image, sync point
