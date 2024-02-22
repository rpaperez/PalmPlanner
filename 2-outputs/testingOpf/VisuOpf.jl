
using PlantGeom
using GLMakie
# using CairoMakie
using Colors
using MultiScaleTreeGraph
using ColorSchemes
# using JSServe
# Page(exportable=true, offline=true)

GLMakie.activate!()
mtg = read_opf("opf/5_1_MAP_36.opf")
viz(mtg)

## 15/03
vspad = Dict(5 => 63.2, 7 => 76.3, 9 => 72.5, 11 => 65.4, 13 => 60.6, 15 => 57.3, 17 => 44.7, 19 => 46.9)
d = DataFrame(mtg)

# node = get_node(mtg, 5)\

traverse!(mtg) do x
    if x.MTG.symbol == "Leaf"
        x[:SPAD] = vspad[x.id]
    end
    if x.MTG.symbol == "Pot"
        x[:SPAD] = 0
    end
    if x.MTG.symbol == "Bulb"
        x[:SPAD] = 30
    end
end

# transform!(mtg, :Ri_PAR_f => (x -> x * 4.57) => :Rad, ignore_nothing=true)
f, a, p = viz(mtg, color=:SPAD, colorscheme=:RdYlGn_11)
colorbar(f[1, 2], p)
set_theme!(backgroundcolor=:black, legendcolor=:white)
theme_dark()

