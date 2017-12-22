# #require(ggplot2)
# require(plotly)
# data(gapminder, package = "gapminder")
# gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
# 	geom_point(aes(size = pop, frame = year, ids = country)) +
# 	scale_x_log10()
# ggplotly(gg)


ssf = function(x){ # sum so far -- use it to build times for next customer
	l = length(x)
	y = numeric(l)
	for (i in 1:l){
		y[i] = sum(x[1:i])
	}
	return(y)
}

# x <- structure(list(pos = array(),
# 					type='coordinate'), class="coordinate")

# what we want is to save all the x,y values in a data frame
# like column 1 is x1 and then whenever x2 comes in start writing
# want x vs time, and y vs time
# currently we're burning through every point and just writing on top of it.

# stall1 = 1000000
# stall2 = 50000
entity.pch = 16
entity.col = "black"
background.col = "white"
busy.col = "green"
idle.col = "red"
text.col = "blue"

set.seed(1)
start = 0.0
stop = 50.0
infinity = 100.0 * stop

#  lambda = 1
lambda = 1 / 2

area.node = 0
area.queue = 0
area.service = 0
index  = 0
number = 0


t.current    = start
# customer arrival based on exponential distribution
temp    = rexp(2 * infinity / lambda, lambda) # reasonable number of arrivals
temp2 = ssf(temp) # get the sums of times
t.arrival = temp2[temp2 < infinity] # we'll truncate to only have the number of arrivals
ids = 1:length(t.arrival) # id values to introduce to plot
# probably don't need any more, bc we have times affixed
t.completion = infinity
# time tracker (?)
t.clock      = start

d = data.frame(x = .1, y = 9, time = t.arrival, alpha = 0, # make invisible
			   size = 1, ease = 'linear', id = ids, stringsAsFactors = F)

x = c(0, 0)
y = c(0, 0)
par(bty = "l", cex = 0.9, lty = 1, font = 3,
	mai = c(0.0, 0.0, 0.0, 0.0), las = 1)
plot(x, y, type = "l", cex = 0.55, xlab="", ylab="",
	 xlim = c(0, 10), xaxp = c(0, 1, 1),
	 ylim = c(0, 10), yaxp = c(0, 1, 1), axes = F, font.axis = 3)
text(5, 9, "Single server queue animation", col = text.col, cex = 1.95, adj = 0.5, font = 10)
text(2, 2, "Number in system:", col = text.col, cex = 1.95, adj = 0.5, font = 10)
text(4, 2, 0, col = text.col, cex = 1.95, adj = 0.5, font = 10)
text(7, 2, "Time:", col = text.col, cex = 1.95, adj = 0.5, font = 10)
text(7, 7, "Number served:", col = text.col, cex = 1.95, adj = 0.5, font = 10)
text(9, 7, 0, col = text.col, cex = 1.95, adj = 0.5, font = 10)
text(8, 2, 0, col = text.col, cex = 1.95, adj = 0.5, font = 10)
lines(c(4, 7.8, 7.8), c(3, 3, 3.4))
xserver = c(8.9, 9.9, 9.9, 8.9)
yserver = c(3, 3, 4, 4)
polygon(xserver, yserver, col = idle.col, border = F)


while ((t.arrival < stop) || (number > 0)) {
	#Sys.sleep(.2)
	t.next = min(t.arrival, t.completion, t.clock)
	if (number > 0) {
		area.node    = area.node    + (t.next - t.current) * number
		area.queue   = area.queue   + (t.next - t.current) * (number - 1)
		area.service = area.service + (t.next - t.current)
	}
	t.current = t.next
	if (t.current == t.clock) {   # if it's time to increase clock
		t.current = t.current + 1
		t.clock = floor(t.current)
		# whites out and then writes on top of current time value
		text(8, 2, t.clock - 1, col = background.col, cex = 1.95, adj = 0.5, font = 10)
		text(8, 2, t.clock, col = text.col, cex = 1.95, adj = 0.5, font = 10)
	}
	else if (t.current == t.arrival) {  # if we are adding a ball to the system - either get served or get in line
		# increments the number in system
		text(4, 2, number, col = background.col, cex = 1.95, adj = 0.5, font = 10)
		number = number + 1
		text(4, 2, number, col = text.col, cex = 1.95, adj = 0.5, font = 10)
		t.arrival     = t.current + rexp(1, lambda)
		if (t.arrival > stop)  {
			t.last      = t.current
			t.arrival   = infinity
		}
		x.begin = 0.1 # new ball
		y.begin = 9.9
		
		x.end = 8.5 - ifelse(number == 1, 0, (min(number, 5)-1)) # either adds to queue or goes to get served
		y.end = 3.5
		
		# tweenr does this 
		# x = seq(x.begin, x.end, length = 100) # This generates intermediate positions for the ball btw the start and the queue
		# y = seq(y.begin, y.end, length = 100) # won't need this later, just the location changes
		
		# this is drawing in a ball in black, then drawing in that same ball in white immediately after
		for (i in 1:100) {
			points(x[i], y[i], pch = entity.pch, cex = 3.8, adj = 0, col = entity.col)
			#Sys.sleep(.2)
			if(i < 100) points(x[i], y[i], pch = entity.pch, cex = 3.8, adj = 0, col = background.col)
		}
		if (number == 1) {
			t.completion = t.current + runif(1, 1, 2)
			polygon(xserver, yserver, col = busy.col, border = F)
		}
	}
	
	else {
		x = seq(8.5, 8.6, length = 100) # ball leaving the station
		y = seq(3.5, 6.5, length = 100)
		for (i in 1:100) {  # draw it in!
			points(x[i], y[i], pch = entity.pch, cex = 3.8, adj = 0, col = entity.col)
			#Sys.sleep(.2)
			points(x[i], y[i], pch = entity.pch, cex = 3.8, adj = 0, col = background.col)
		}
		# increment the number served
		text(9, 7, index, col = background.col, cex = 1.95, adj = 0.5, font = 10)
		index = index + 1
		text(9, 7, index, col = text.col, cex = 1.95, adj = 0.5, font = 10)
		text(4, 2, number, col = background.col, cex = 1.95, adj = 0.5, font = 10)
		number = number - 1
		text(4, 2, number, col = text.col, cex = 1.95, adj = 0.5, font = 10)
		if (number > 0) {
			t.completion = t.current + runif(1, 1, 2)
			if (number < 6) points(8.5 - number, 3.5, pch = entity.pch, cex = 3.8, adj = 0, col = background.col)
			for (i in 1:100) {
				for (j in 1:number) {
					points(8.5 - j + i / 100, 3.5, pch = entity.pch, cex = 3.8, adj = 0, col = entity.col)
					#Sys.sleep(.2)
					if (i < 100) points(8.5 - j + i / 100, 3.5, pch = entity.pch, cex = 4.1, adj = 0, col = background.col)
				}
			}
		}
		else {
			t.completion = infinity
			polygon(xserver, yserver, col = idle.col, border = F)
			points(8.5, 3.5, pch = entity.pch, cex = 3.8, adj = 0, col = background.col)
		}
	}
}
print(x)
print(c("for ", index, " jobs"))
print(c("   average interarrival time = ", t.last / index))
print(c("   average wait ............ = ", area.node / index))
print(c("   average delay ........... = ", area.queue / index))
print(c("   average service time .... = ", area.service / index))
print(c("   average # in the node ... = ", area.node / t.current))
print(c("   average # in the queue .. = ", area.queue / t.current))
print(c("   utilization ............. = ", area.service / t.current))


