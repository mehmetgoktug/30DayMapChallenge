# 30DayMapChallenge — Day 6: Vintage
# Earthquakes of Türkiye

# Load packages ----------------------------------------------------------------
library("sf")
library("tmap")

tmap_mode("plot")

# Pallete and extent -----------------------------------------------------------
paper <- "#f1e7cd"
land_nb <- "#e8d9b3"
land_tr <- "#e0c592"
lake <- "#cfd6c6"
ink <- "#4a3826"
ink_lt <- "#86714f"
water <- "#5f7f86"
quake <- "#9b3322"

# Bbox
bb <- c(xmin=22, xmax=48, ymin=32.5, ymax=45) |>
    st_bbox(crs="EPSG:4326") |>
    st_as_sfc() |>
    st_transform("EPSG:3857")
bb_map <- c(xmin=25, xmax=45.5, ymin=34.5, ymax=43) |>
    st_bbox(crs="EPSG:4326") |>
    st_as_sfc() |>
    st_transform("EPSG:3857")

# Misc functions ---------------------------------------------------------------
ne <- function(type, category="cultural") {
    x <- rnaturalearth::ne_download(
        scale=10,
        type=type,
        category=category,
        returnclass="sf"
    )
    names(x) <- tolower(names(x))
    st_agr(x) <- "constant"
    x
}

# "BLACK SEA" -> "B L A C K   S E A"
spaced <- function(x) {
    words <- strsplit(x, " ")[[1]]
    words <- vapply(words, 
        \(w) paste(strsplit(w, "")[[1]], collapse="\u2009"),
        ""
    )
    paste(words, collapse="\u2003")
}

halo <- function(col=paper, just="center") {
    opt_tm_text(
        halo=TRUE,
        halo.col=col,
        halo.width=0.12,
        halo.alpha=0.95,
        just=just
    )
}

# Earthquake data --------------------------------------------------------------
dd <- read.csv("earthquake.csv")
d <- st_as_sf(dd, coords=c("Long", "Lat"), crs="EPSG:4326")
d <- st_transform(d, "EPSG:3857")
d$Mag. <- as.numeric(sub("Ms|Mw|MS", "", d$Mag.))
d$Mag.[which(is.na(d$Mag.))] <- c(7.2, 7.7)
d$Deaths <- as.numeric(d$Deaths)

# For English month names
d$Date <- withr::with_locale(
    c(LC_TIME="C"),
    as.Date(d$Date, format="%d %B %Y")
)
d$Year <- as.integer(format(d$Date, "%Y"))

# Most deadly earthquakes
key_pos <- data.frame(
    Place=c("Kahramanmaraş", "Erzincan", "İzmit"),
    lon  =c(36.70, 39.20, 30.00),
    lat  =c(37.30, 39.65, 41.80),
    just =c("right", "right", "center")
)
key_lab <- key_pos |>
    merge(d[, c("Place", "Mag.", "Year")], by="Place") |>
    sort_by(~list(-Mag.)) |>
    _[1:3, ] |>
    st_sf() |>
    st_transform("EPSG:3857")
print(key_lab)

key_lab$lab <- paste0(key_lab$Place, ", ", key_lab$Year, "\nM ", 
                      format(key_lab$Mag., nsmall=1))

# For base map -----------------------------------------------------------------
countries <- ne("countries") |>
    st_make_valid() |>
    st_transform("EPSG:3857") |>
    st_crop(bb)
borders <- ne("admin_0_boundary_lines_land") |>
    st_transform("EPSG:3857") |>
    st_crop(bb)
lakes <- ne("lakes", "physical") |>
    st_make_valid() |>
    st_transform("EPSG:3857") |>
    st_crop(bb)
rivers <- ne("rivers_lake_centerlines", "physical") |>
    st_crop(st_transform(bb, "EPSG:4326")) |>
    st_transform("EPSG:3857")

tur <- countries[countries$adm0_a3 == "TUR", ]
nb <- countries[countries$adm0_a3 != "TUR", ]
borders <- borders[grepl("International", borders$featurecla), ]
lakes <- lakes[lakes$name %in% c("Lake Van", "Lake Tuz", "Beyşehir", 
                                 "Eğirdir", "Lake Urmia", "Lake Sevan"), ]
rivers$w <- ifelse(rivers$scalerank <= 4, 0.9, 0.55)

# Coast effects
land <- st_union(countries) |> st_simplify(dTolerance=0.08)
wash <- st_sf(geometry=do.call(c, lapply(c(2.2, 1.2, 0.5) * 30e3, 
                                         \(r) st_buffer(land, r))))
ripples <- st_sf(
    alpha=c(0.65, 0.45, 0.3, 0.17), 
    geometry=do.call(c, lapply(c(25, 65, 115, 18) * 10e3,
                               \(r) st_boundary(st_buffer(land, r))))
)
coast <- st_boundary(land)

# Labels -----------------------------------------------------------------------
seas <- data.frame(
    name=c(spaced("BLACK SEA"), 
           spaced("ÆGEAN SEA"), 
           spaced("MEDITERRANEAN SEA")),
    lon=c(33.2, 25.42, 30.45),
    lat=c(42.6, 38.70, 35.42),
    angle=c(0, 90, 0)
) |>
    st_as_sf(coords=c("lon", "lat"), crs="EPSG:4326") |>
    st_transform("EPSG:3857")

nb_lab <- data.frame(
    name = vapply(c("BULGARIA", "GREECE", "GEORGIA", "ARMENIA",
                    "IRAN", "IRAQ", "SYRIA", "CYPRUS"), 
        spaced, 
        ""
    ),
    lon=c(26.5, 25.75, 43.2, 44.30, 44.83, 43.4, 38.7, 33.25),
    lat=c(42.35, 41.15, 42.15, 40.40, 38.85, 36.85, 35.75, 35.12)
) |>
    st_as_sf(coords=c("lon", "lat"), crs="EPSG:4326") |>
    st_transform("EPSG:3857")

tr_lab <- data.frame(name=spaced("TÜRKİYE"), lon=34.6, lat=38.3) |>
    st_as_sf(coords=c("lon", "lat"), crs="EPSG:4326") |>
    st_transform("EPSG:3857")

# Map --------------------------------------------------------------------------
ripple_layers <- Reduce(`+`, 
    lapply(seq_len(nrow(ripples)), 
        \(i) tm_shape(ripples[i, ]) + 
            tm_lines(col=water, lwd=0.4, col_alpha=ripples$alpha[i])
    )
)

m <- tm_shape(wash, bbox=bb_map) +
    tm_fill(fill=water, fill_alpha=0.05) +
    ripple_layers +
    tm_shape(nb) +
    tm_fill(fill=land_nb) +
    tm_shape(tur) +
    tm_fill(fill=land_tr) +
    tm_shape(lakes) +
    tm_polygons(fill=lake, col=water, lwd=0.35) +
    tm_shape(rivers) +
    tm_lines(col=water, lwd="w", lwd.scale=tm_scale_asis(), 
             lwd.legend=tm_legend_hide()) +
    tm_shape(borders) +
    tm_lines(col=ink, lwd=0.55, lty="42") +
    tm_shape(coast) +
    tm_lines(col=ink, lwd=0.5) +
    tm_graticules(
        x=seq(26, 44, 2),
        y=seq(36, 42, 2),
        col=ink,
        alpha=0.3,
        lwd=0.5,
        labels.size=0.55,
        labels.col=ink_lt
    ) +
    tm_shape(tr_lab) +
    tm_text(
        text="name",
        size=1.6,
        col=ink,
        col_alpha=0.25,
        fontfamily="IM FELL English SC"
    ) +
    tm_shape(seas) +
    tm_text(
        text="name",
        size=0.58,
        col=water,
        angle="angle",
        fontfamily="IM FELL English",
        fontface="italic",
        options=halo(paper)
    ) +
    tm_shape(nb_lab) +
    tm_text(
        text="name",
        size=0.48,
        col=ink_lt,
        fontfamily="EB Garamond",
        options=halo(land_nb)
    ) +
    tm_shape(d) +
    tm_symbols(
        size="Mag.",
        size.scale=tm_scale_intervals(
            breaks=c(5, 6, 6.5, 7, 7.5, 8),
            values=c(0.25, 0.38, 0.54, 0.74, 1),
            labels=c("5.0 – 5.9",
                     "6.0 – 6.4",
                     "6.5 – 6.9",
                     "7.0 – 7.4",
                     "7.5 – 7.8")
        ),
        size.legend=tm_legend(
            title="Magnitude",
            position=tm_pos_in("right", "bottom"),
            frame=TRUE,
            frame.color=ink,
            frame.lwd=0.8,
            frame.r=0,
            bg.color=paper,
            bg.alpha=1,
            title.fontfamily="IM FELL English SC",
            title.size=0.75,
            title.color=ink,
            text.fontfamily="EB Garamond",
            text.size=0.58,
            text.color=ink
        ),
        shape=21,
        fill=quake,
        fill_alpha=0.72,
        col="#4d150b",
        lwd=0.6
    ) +
    tm_shape(key_lab) +
    tm_text(
        text="lab",
        size=0.62,
        col=ink,
        fontfamily="EB Garamond",
        fontface="italic",
        options=halo(paper, just="right")
    ) +
    # tm_compass(
    #     type="8star",
    #     size=3,
    #     position=tm_pos_in(
    #         just.h="center",
    #         just.v="center"
    #     ),
    #     color.dark=ink,
    #     color.light=paper,
    #     text.color=ink,
    #     text.size=0.7
    # ) +
    # tm_scalebar(
    #     breaks=c(0, 100, 200),
    #     text.size=0.5,
    #     color.dark=ink,
    #     color.light=paper,
    #     text.color=ink,
    #     position=tm_pos_in("left", "bottom")
    # ) +
    tm_title_out("EARTHQUAKES OF TÜRKİYE",
        size=2.1,
        color=ink,
        fontfamily="IM FELL English SC",
        position=tm_pos_out("center", "top", pos.h="center", align.h="center")
    ) +
    tm_title_out("Shewing the epicentres of the destructive shocks, 1903 – 2025",
        size=0.95,
        color=ink_lt,
        fontfamily="EB Garamond",
        fontface="italic",
        position=tm_pos_out("center", "top", pos.h="center", align.h="center")
    ) +
    tm_credits("#30DayMapChallenge · Day 6: Vintage",
        size=0.55,
        color=ink_lt,
        position=tm_pos_out("center", "bottom", pos.h="left")
    ) +
    tm_credits("Data: Wikipedia, List of earthquakes in Türkiye · Natural Earth",
        size=0.55,
        color=ink_lt,
        position=tm_pos_out("center", "bottom", pos.h="right")
    ) +
    tm_layout(
        bg.color=paper,
        outer.bg.color=paper,
        frame=TRUE,
        frame.color=ink,
        frame.lwd=1.4,
        frame.double_line=TRUE,
        inner.margins=c(0, 0, 0, 0),
        outer.margins=c(0.015, 0.02, 0.015, 0.02),
        text.fontfamily="EB Garamond",
        attr.color=ink
    ) +
    tm_crs("auto")

tmap_save(m, "earthquakes_vintage.png", width=2600, height=1840, dpi=300)
