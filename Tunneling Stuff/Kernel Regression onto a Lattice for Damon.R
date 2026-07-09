library(dplyr)
library(jsonlite)

pbp_2024_2_pitches_w_trajectories = read.csv("pbp_2024_2_pitches_w_trajectories.csv",h=T)

pbp_2024_2_pitches_w_trajectories <- pbp_2024_2_pitches_w_trajectories %>%
  mutate(trajectory = lapply(trajectory, fromJSON))

# Verify the transformation
str(pbp_2024_2_pitches_w_trajectories$trajectory)

#Assuming measurements are uniformly spaced over time_of_flight variable?
#confirm with Damon.

pitch1 = data.frame(t = seq(0,pbp_2024_2_pitches_w_trajectories$time_of_flight[1],length.out=50),
				x = pbp_2024_2_pitches_w_trajectories$trajectory[[1]]$x,
				y = pbp_2024_2_pitches_w_trajectories$trajectory[[1]]$y,
				z = pbp_2024_2_pitches_w_trajectories$trajectory[[1]]$z)


pitch2 = data.frame(t = seq(0,pbp_2024_2_pitches_w_trajectories$time_of_flight[2],length.out=50),
				x =  pbp_2024_2_pitches_w_trajectories$trajectory[[2]]$x,
				y =  pbp_2024_2_pitches_w_trajectories$trajectory[[2]]$y,
				z =  pbp_2024_2_pitches_w_trajectories$trajectory[[2]]$z)

#t1 will be the shared temporal lattice upon which we regress our positions onto.
#x1,y1,z1 and x2,y2,z2 will be the regressed positions of our two respective position upon our temporal lattice
#d1 is euclidean distance between our two positions over our lattice
#Would like a simple plot of t1 against d1.  The integral under d1 (approximated by a riemann sum)
#is the distance we're interested in. (Notice the units for this distance is feet*seconds
#h1 and h2 are the bandwidths for each kernel estimator.  Leave one out cross validation is struggling
#Right now I've just input values that are working, will need a more robust estimator for the bandwidth.
#Currently set at about 1/4 of a standard deviation for t.

t1 = seq(0,max(c(pitch1$t,pitch2$t)),length.out=200)
x1 = rep(0,200)
x2 = rep(0,200)
y1 = rep(0,200)
y2 = rep(0,200)
z1 = rep(0,200)
z2 = rep(0,200)

n = length(pitch1$x)

#CV = function(h){
#	SSR = 0
#	for(i in 1:n){
#		y1 = sum(exp(-.5*((x[i]-x[-i])/h)^2)*y[-i])/sum(exp(-.5*((x[i]-x[-i])/h)^2))
#		resid = y[i] - y1
#		SSR = SSR + resid^2
#	}
#	SSR
#}


h1 = .01
h2 = .01

#Kernel Regression onto the lattice

for(i in 1:200){
	x1[i] = sum(exp(-.5*((t1[i]-pitch1$t)/h1)^2)*pitch1$x)/sum(exp(-.5*((t1[i]-pitch1$t)/h1)^2))
	y1[i] = sum(exp(-.5*((t1[i]-pitch1$t)/h1)^2)*pitch1$y)/sum(exp(-.5*((t1[i]-pitch1$t)/h1)^2))
	z1[i] = sum(exp(-.5*((t1[i]-pitch1$t)/h1)^2)*pitch1$z)/sum(exp(-.5*((t1[i]-pitch1$t)/h1)^2))
	x2[i] = sum(exp(-.5*((t1[i]-pitch2$t)/h2)^2)*pitch2$x)/sum(exp(-.5*((t1[i]-pitch2$t)/h2)^2))
	y2[i] = sum(exp(-.5*((t1[i]-pitch2$t)/h2)^2)*pitch2$y)/sum(exp(-.5*((t1[i]-pitch2$t)/h2)^2))
	z2[i] = sum(exp(-.5*((t1[i]-pitch2$t)/h2)^2)*pitch2$z)/sum(exp(-.5*((t1[i]-pitch2$t)/h2)^2)) 
}

distance.df = data.frame(t1,d1 = rep(0,200),x1,y1,z1,x2,y2,z2)
distance.df$d1 = sqrt((distance.df$x1 - distance.df$x2)^2 + (distance.df$y1 - distance.df$y2)^2 + (distance.df$z1 - distance.df$z2)^2)
plot(distance.df$t1,distance.df$d1,type="l")

#hmm, wierd tail area behavior, thinking it has to do with us choosing the maximum reaction time vs the minimum.

delta = distance.df$t1[2] - distance.df$t1[1]
distance = sum(delta*distance.df$d1)


