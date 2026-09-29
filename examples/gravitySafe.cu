__device__ double gravitySafe(double m1,double m2,double r2) {
	return ((r2 == 0.0) ? 0.0 : ((m1 * m2) / r2))
}
