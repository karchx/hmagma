__device__ double gravitySafe(doublem1, doublem2, doubler2) {
label_0:
	double t_0 = 0.0;
	double t_1 = (r2 == 0.0);
	if (t_1) {
		goto label_1;
	} else {
		goto label_2;
	}
label_1:
	double t_3 = 0.0;
	double t_2 = 0.0;
	goto label_3;
label_2:
	double t_4 = (m1 * m2);
	double t_5 = (t_4 / r2);
	double t_2 = t_5;
	goto label_3;
label_3:
	return t_2;
}


__device__ double main() {
label_0:
	double t_0 = NULL;
	return NULL;
}
