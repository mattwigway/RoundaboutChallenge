#' @export
draw_roundabout = function (island_radius, approach_radius, road_width, veh_width=2, buffer=1.5,
    lwscale=3, splitter_width=2, splitter_length=7, land="beige", veh_color="#4b9cd3") {
    corner_offset = (island_radius + approach_radius + road_width) / sqrt(2)
    circles = tibble(
        x = c(0, corner_offset, corner_offset, -corner_offset, -corner_offset),
        y = c(0, corner_offset, -corner_offset, corner_offset, -corner_offset),
        radius = c(island_radius, approach_radius, approach_radius, approach_radius, approach_radius)
    )

    entry_distance = corner_offset * 3

    approaches = rbind(
        mutate(circles[2:nrow(circles),], type="approach"),
        mutate(circles[2:nrow(circles),], type="exit")
    ) |>
    mutate(
        xend = case_when(
            # horizontal ones
            type == "approach" & x < 0 & y < 0 | type == "exit" & x < 0 & y > 0 ~ -entry_distance,
            type == "approach" & x > 0 & y > 0 | type == "exit" & x > 0 & y < 0 ~ entry_distance,
            TRUE ~ sign(x) * (road_width + island_radius / 2)
        ), 
        yend = case_when(
            # horizontal ones
            type == "exit" & x > 0 & y > 0 | type == "approach" & x < 0 & y > 0 ~ entry_distance,
            type == "exit" & x < 0 & y < 0 | type == "approach" & x > 0 & y < 0 ~ -entry_distance,
            TRUE ~ sign(y) * (road_width + island_radius / 2)
        )
    )

    # splitters
    splitters = tribble(
        ~dir, ~x, ~y,
        "w", -(island_radius + road_width), splitter_width / 2,
        "w", -(island_radius + road_width), -splitter_width / 2,
        "w", -(island_radius + road_width + splitter_length), 0,
        "e", (island_radius + road_width), splitter_width / 2,
        "e", (island_radius + road_width), -splitter_width / 2,
        "e", (island_radius + road_width + splitter_length), 0,
        "n", splitter_width / 2, (island_radius + road_width),
        "n", -splitter_width / 2, (island_radius + road_width),
        "n", 0, (island_radius + road_width + splitter_length),
        "s", splitter_width / 2, -(island_radius + road_width),
        "s", -splitter_width / 2, -(island_radius + road_width),
        "s", 0, -(island_radius + road_width + splitter_length)
    )

    path = calc_path(island_radius, approach_radius, road_width, veh_width, buffer, splitter_width)

    path_arcs = tribble(
        ~name, ~x, ~y, ~r, ~start, ~end,
        "approach_island", 0, path$swept_island_y, path$swept_island_radius, pi, pi + path$swept_island_angle,
        "approach", path$swept_approach_x, path$swept_approach_y, path$swept_approach_radius, 0, path$swept_approach_angle,
        "dep_island", 0, path$swept_island_y, path$swept_island_radius, pi, pi - path$swept_island_angle,
        "dep", -path$swept_approach_x, path$swept_approach_y, path$swept_approach_radius, 2 * pi - path$swept_approach_angle, 2*pi

    )

    path_lines = tribble(
        ~x, ~y, ~xend, ~yend,
        -entry_distance, -(veh_width + buffer) / 2, path$swept_approach_x, -(veh_width + buffer + splitter_width) / 2,
        entry_distance, -(veh_width + buffer) / 2, -path$swept_approach_x, -(veh_width + buffer + splitter_width) / 2,
    )

    crosswalks = tribble(
        ~x, ~y,
        corner_offset, corner_offset,
        0, corner_offset + splitter_length / 2,
        -corner_offset, corner_offset,
        -corner_offset - splitter_length / 2, 0,
        -corner_offset, -corner_offset,
        0, -corner_offset - splitter_length / 2,
        corner_offset, -corner_offset,
        corner_offset + splitter_length / 2, 0,
        corner_offset, corner_offset
    )

    # two parallel lines
    crosswalks = rbind(
        crosswalks |> mutate(x=x * (corner_offset - 4) / corner_offset, y=y * (corner_offset - 4) / corner_offset, which="inner"),
        crosswalks |> mutate(which="outer")
    )

    # ground
    ground = tribble(
        ~group, ~x, ~y,
        "nw", -entry_distance, entry_distance,
        "nw", -road_width, entry_distance,
        "nw", -corner_offset + approach_radius, corner_offset,
        "nw", -corner_offset, corner_offset - approach_radius,
        "nw", -entry_distance, road_width,

        "ne", entry_distance, entry_distance,
        "ne", road_width, entry_distance,
        "ne", corner_offset - approach_radius, corner_offset,
        "ne", corner_offset, corner_offset - approach_radius,
        "ne", entry_distance, road_width,

        "sw", -entry_distance, -entry_distance,
        "sw", -road_width, -entry_distance,
        "sw", -corner_offset + approach_radius, -corner_offset,
        "sw", -corner_offset, -corner_offset + approach_radius,
        "sw", -entry_distance, -road_width,

        "se", entry_distance, -entry_distance,
        "se", road_width / 2, -entry_distance,
        "se", corner_offset - approach_radius, -corner_offset,
        "se", corner_offset, -corner_offset + approach_radius,
        "se", entry_distance, -road_width
    )

    plot = ggplot() +
        # draw crosswalks
        geom_path(data = crosswalks, aes(x, y, group=which), color="white", linewidth=lwscale) +
        geom_polygon(data = ground, aes(x, y, group=group), color=land, fill=land) +
        # draw center island and approach radii
        geom_circle(data = circles, aes(x0=x, y0=y, r=radius), fill=land, color=land) +
        #geom_segment(data = approaches, aes(x=x, y=y, xend=xend, yend=yend), color=land, linewidth=approach_radius * lwscale) +
        geom_polygon(data = splitters, aes(x=x, y=y, group=dir), fill=land, color=land) +
        geom_arc(data=path_arcs, aes(x0=x, y0=y, r=r, start=start, end=end), linewidth = veh_width * lwscale, color=veh_color, lineend="round") +
        geom_segment(data=path_lines, aes(x=x, y=y, xend=xend, yend=yend), linewidth=veh_width * lwscale, color=veh_color) +
        coord_fixed() +
        theme_void() +
        theme(plot.background=element_rect(fill="black")) +
        scale_x_continuous(expand=c(0, 0)) +
        scale_y_continuous(expand=c(0, 0))

    return(list(
        path=path,
        plot=plot,
        speeds = calc_speeds(path)
    ))
}

#' path calculation
#' @export
calc_path = function (island_radius, approach_radius, road_width, veh_width, buffer, splitter_width) {
    # Calculate straight-through path W-E. Presumably this will be the fastest
    # path through the roundabout so will control speed.

    # So the idea here is first find the controlling radius for the turn
    # around the island. We know that the fastest path will pass due 
    # south of the island as close as possible, and will be tangent to
    # it at that point. So start from there and figure out what size
    # circle will place the vehicle as close as possible to the approach
    # radius, as that should also be on the fastest path.

    # radius to center of vehicle tangent point
    effective_island_radius = island_radius + buffer + veh_width / 2
    effective_approach_radius = approach_radius + buffer + veh_width / 2

    corner_offset = (island_radius + approach_radius + road_width) / sqrt(2)
    # find the center of the inner circle that places the vehicle as
    # close as possible to the approach while being tangent
    # to the center island. This point is the tip of a right
    # triangle where the right angle is at (0, -corner_offset),
    # one of the other points is at (-corner_offset, -corner_offset),
    # the hypotenuse is the island_swept_radius + effective_approach_radius,
    # and the vertical side is island_swept_radius - effective_island_radius + corner_offset
    # Solve for island_swept_radius:

    swept_island_radius =
        (-effective_approach_radius^2 + effective_island_radius^2 - 2 * effective_island_radius * corner_offset + 2 * corner_offset^2) /
        (2 * (effective_approach_radius + effective_island_radius - corner_offset))

    swept_island_y = swept_island_radius - effective_island_radius

    swept_island_angle = atan2(swept_island_y + corner_offset, corner_offset)

    # Now, swept approach radius/angle
    # figure out where it links up with other path
    transition_x = -swept_island_radius * cos(swept_island_angle)
    transition_y = swept_island_y - swept_island_radius * sin(swept_island_angle)

    # NB having splitter width here is a hack as it puts the vehicle off-center in the
    # lane (offset by the splitter) while still traveling straight. But that's not
    # going to be very visually noticeable.
    target_y = -(veh_width + buffer + splitter_width) / 2

    distance_to_x_axis_perpendicular_to_path_at_transition = abs((transition_y - target_y) / cos(pi / 2 - swept_island_angle))

    swept_approach_radius = abs(distance_to_x_axis_perpendicular_to_path_at_transition * cos(pi / 2 - swept_island_angle) /
        (cos(pi / 2 - swept_island_angle) - 1))

    swept_approach_x = transition_x - swept_approach_radius * sin(pi / 2 - swept_island_angle)
    swept_approach_y = transition_y - swept_approach_radius * cos(pi / 2 - swept_island_angle)

    return(list(
        swept_island_radius = swept_island_radius,
        swept_island_y = swept_island_y,
        swept_island_angle = pi / 2 - swept_island_angle,
        transition_x = transition_x,
        transition_y = transition_y,
        swept_approach_radius = swept_approach_radius,
        swept_approach_angle = pi / 2 - swept_island_angle,
        swept_approach_x = swept_approach_x,
        swept_approach_y = swept_approach_y
    ))
}

#' @export
calc_speeds = function (path) {
    # NCHRP, p. 9-11
    return(list(
        approach_speed = 5.5374 * (path$swept_approach_radius / 0.3048) ^ 0.3861,
        circ_speed = 5.5693 * (path$swept_island_radius / 0.3048) ^ 0.3673
    ))
}