# ggh4x (development version)

* Fixed misplacement of nest lines in vertical strips (#50).
* Fixed bug in setting aspect ratio in `facet_grid2()` and family (#56).

# ggh4x 0.2.0

## Facets

Facets have largely been refactored. Accordingly, the 
[vignette](https://teunbrand.github.io/ggh4x/articles/Facets.html) has been 
updated to reflect the latest changes.

### Wrap

The previously existing `facet_wrap2()` now has the `trim_blank` option to force
`nrow` and `ncol` argument if these exceed the number of rows or columns needed
to place all the panels. Thanks to @coolbutuseless for letting me steal the idea
from the [{facetious}](https://github.com/coolbutuseless/facetious) package. 
The `facet_wrap2()` function was already used to offer more options over axis 
drawing.

### Grid

In addition, the cousin `facet_grid2()` makes an entrance. It offers the same 
axis drawing extensions as `facet_wrap2()`, but for the grid layout. Moreover, 
it allows for independent scales across  rows and columns, which is a constraint 
in `ggplot2::facet_grid()`.

### Nested facets

Besides `facet_nested_wrap()` inheriting from `facet_wrap2()`, which was already
the case, now `facet_nested()` also inherits from `facet_grid2()` to make use
of the extended axis drawing options. Also, the `nest_line` argument now takes
a `element_line()` or `element_blank()` to draw the indicator instead of the 
`logical(1)` argument it took previously. The `bleed` argument has been moved
from the nested facet to the new `strip_nested()` function (see below).

### Manual

New in the family of facets is now `facet_manual()`: a facet that can take
a user specified design for a layout and populate the panels accordingly.

### Strips

The facet functions in ggh4x now have `strip` arguments, which can be used
with new `strip_*()` functions that control how the strips are drawn. For now,
they come in the following three variants:

* `strip_vanilla()` which draws normal strips, but allows you to control whether
  labels should be clipped and has the `size` argument that can let strips on
  different layers have different sizes. This can be convenient when the strip
  labels have very different sizes.
* `strip_themed()` does all of the above, but also let's you assign 
  `element_text()` and `element_rect()` to different strips, allowing you to
  have greater control of their styling. These elements can be assigned 
  independently for the horizontal and vertical strips and can be applied to 
  single strips or layers of strips.
* `strip_nested()` again does all of the above, but also merges the strips with
  the same labels if they are next to one another. This means that the core
  functionality of `facet_nested()` and `facet_nested()` wrap has been moved
  and is now powered by the `strip` argument. They can still be convenient to
  draw nesting indicators.

## Other changes

* Fixed spelling in README (#32, thanks @vikatti!)
* Added helper for secondary axes: help_secondary()
* Added stat_difference() for shading a ribbon based on the sign of difference.
* Included 'colour'/'color' arguments for axis guides for recolouring the
  whole axis in one go (instead of editing 3-5 theme elements).
* Fix bug in facetted_pos_scales() with date scales (#37)
* Fix bug in nested facets when `strip.text = element_blank`. Requires ggplot2
  >3.3.0, or the dev version at time of writing. (#35)
* Changed facet_wrap2()/facet_nested_wrap() 'free', 'axes' and 'remove_labels'
  arguments to be less ambiguous.
* Fix bug in truncated axes with discrete scales (#39).
* Fix bug in geom_pointpath() with 1-member groups (#43).
* New axis function `guide_axis_manual()`.

# ggh4x 0.1.2.1

* Updated functions for ggplot2 v3.3.0 (#1)
* Fixed compatibility issue for facet_nested() and patchwork (#4)
* Fixed bug in facet_nested(bleed = FALSE) (#7)
* Added scale_(x/y)_dendrogram() and associated guide guide_dendro() (#1)
* Added guide_axis_nested() and convenience function weave_factors() (#3)
* Added guide_axis_minor() for minor break tickmarks
* Added guide_axis_logticks for logarithmic tickmarks
* Added element_part_rect() for using rectangles with a subset of edges as theme
  element (#13).
* Added stat_rollingkernel() for different smoothing lines.
* Added stat_rle() for runlength calculation.
* Fixed bug in geom_pointpath() (#15)
* Fixed scale_(x/y)_dendrogram so that labels are passed from scale (#17)
* geom_pointpath() now curves with nonlinear coordinates (#15).
* Added stat_funxy(), stat_centroid() and stat_midpoint() (#16).
* Added facet_wrap2() with few extensions.
* Added facet_nested_wrap() for merging strips (#19)
* Alternative specification of facet scales in `facetted_pos_scales` through 
  formulas (#25).
* Added option to discard dendrogram labels (#23).
* Added coloured text legend (stringlegend, #31).
* Added truncated axis guide.
* Supported axis truncation in other position guides too.

# ggh4x 0.1.1

* Package should be in usable state
* Added facet_nested()
* Added facetted_pos_scales()
* Added geom_pointpath()
* Added geom_rectrug()
* Added position_disjoint_ranges()
* Added position_lineartrans()
* Added scale_listed()
* Added scale_(fill/colour)_multi()
* Added stat_theodensity()

# ggh4x 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Initialised repository skeleton
