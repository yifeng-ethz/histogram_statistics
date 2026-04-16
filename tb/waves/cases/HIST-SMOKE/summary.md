# HIST-SMOKE

Directed bring-up waveform for the standalone `hist_smoke_test`.

## Intent

- show the clean reset, configuration-apply, first fill hit, and first bank-swap readout path
- provide a minimal reference waveform for CSR sequencing, queue ingress, and frozen-bank burst read debug
- anchor later promoted long-run cases to a known-good deterministic baseline

## Notes

- the source VCD is generated from the UVM harness with full `tb_top` scope so clock/reset and DUT state are both visible
- the checked-in GTKWave save file is a reusable template; a case-specific polished `.gtkw` can be derived from it if needed
