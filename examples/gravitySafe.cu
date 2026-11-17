__device__ double gravitySafe(double m1, double m2, double r2) {
	double t_0;
	double t_1;
	double t_2;
	double t_3;
	double t_4;
	double t_5;
label_0:
	t_0 = 0.0;
	t_1 = (r2 == 0.0);
	if (t_1) {
		goto label_1;
	} else {
		goto label_2;
	}
label_1:
	t_3 = 0.0;
	t_2 = 0.0;
	goto label_3;
label_2:
	t_4 = (m1 * m2);
	t_5 = (t_4 / r2);
	t_2 = t_5;
	goto label_3;
label_3:
	return t_2;
}
