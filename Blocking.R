# Blocking
# Isolate pixels
blocking <- function(q, dataset = training) {
	data <- dataset[ , 2:785]
	pixels <- ncol(data) # number of total pixels (784)
	nc <- floor(sqrt(pixels)) # number of columns
	r = floor(nc / q) # number of rows per quadrant
	takeaway <- nc - (r * q) # number of rows/cols will be left over
	initial.mat <- matrix(1:(nc^2), nrow = 28, byrow = TRUE) # initial matrix that is nc X nc
	nums <- initial.mat[1:(nrow(initial.mat) - takeaway), 1:(ncol(initial.mat) - takeaway)] # If there are left over rows/cols, get rid of those variable nums (almost all whitespace)
	nums <- as.numeric(nums)
	data <- data[ , nums]
	pixels <- ncol(data)
	nc <- floor(sqrt(pixels))
	r = floor(nc / q) # number of rows per quadrant
	mat <- matrix(0, nrow = nrow(data), ncol = q * q) # Initialize matrix that has a new column for each q X q variable

	# Start looping
	counter <- 1
	for (qc in 0:(q-1)) {
		for (qr in 0:(q-1)) {
			rows <- NULL
			for (ri in 0:(r-1)) {
				 rows <- c(rows, ((qr * r + ri)*nc+1+qc*r):((qr * r + ri)*nc+r+qc*r)) # compute which rows and columns belong in sub-square
			}
			mat[ , counter] <- rowSums(data[ , rows])
			counter <- counter + 1
		}
	}
	mattable <- as.data.frame(mat)
}

# Usage:
# blocking(num_rows_preferred, dataset)
fourteen <- blocking(14, training)
twelve <- blocking(12, training)
seven <- blocking(7, training)
four <- blocking(4, training)