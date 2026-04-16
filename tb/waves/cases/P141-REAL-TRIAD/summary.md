# P141-REAL-TRIAD

Promoted long-run waveform for the `hist_real_hit_rand_test` triad profile.

## Intent

- capture the three promoted real-hit phases that closed toggle coverage for routine signoff
- show queue occupancy growth, drain, and bank-swap behavior under sustained MuTRiG-format traffic
- provide the publication anchor for the measured coverage-vs-wall-clock plateau decision

## Notes

- this case uses `+HIST_REAL_PROFILE=triad +HIST_REAL_HITS=2048`
- the waveform is publication-oriented rather than exhaustive; the detailed coverage trends remain in `tb/uvm/cov_runs/signoff_longrun_v1/`
