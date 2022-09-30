#We assumed the potential for spread between adjacent pixels a and b was proportional to the product of the population densities, so that pandemics were likely to travel along paths of high population density. To estimate the relative chance of a source pixel image x resulting in spread to a destination pixel image y, we converted the pixel image to a network using 4-connectivity, with pixels representing nodes and edge weights between adjacent pixels a and b given by


#where ra is the population density in pixel a. The potential of pan- demic spread from pixel x to pixel y was then estimated by the shortest path s(x, y) in the graph between corresponding nodes x and y, which was found using Dijkstra’s algorithm [26]. The rela- tive chance of pandemic spread to pixel ywas then estimated using

#where the sum is taken over all potential source pixels x. To assess the potential of each source pixel x to contribute to
#a pandemic, we use

#so that those pixels with highpopulations at risk connected to large populations have the most influence on pandemic projections.
