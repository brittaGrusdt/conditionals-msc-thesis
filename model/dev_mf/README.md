This is MF's dev corner.

### 01_avg_listener_based_EU.wppl

- model developed to solve causal inference problem ("A=>C" should suggest network "A?->C?")
	- speaker passes 'causalNet' to LL
	-EUs are derived from expected values of probTable of LL (given causalNet)
	- priors for generation of states are asymetrically skewed
		- this latter aspect seems to be what really drives the (positive) result
