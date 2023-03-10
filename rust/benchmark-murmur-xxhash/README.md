# benchmark-murmur-xxhash

## The Result

I've benchmarked this on GitPod:

```
murmur3                 time:   [8.6240 ns 8.6546 ns 8.6834 ns]
Found 12 outliers among 100 measurements (12.00%)
  1 (1.00%) low severe
  10 (10.00%) low mild
  1 (1.00%) high mild

xxhash                  time:   [6.6472 ns 6.6741 ns 6.7011 ns]
Found 3 outliers among 100 measurements (3.00%)
  3 (3.00%) low mild
```

`xxhash` is slightly faster than `murmur3`, on my testing.

## Environment Information

```
$ cat /etc/issue
Ubuntu 20.04.5 LTS \n \l
```

```
$ lscpu
Architecture:                    x86_64
CPU op-mode(s):                  32-bit, 64-bit
Byte Order:                      Little Endian
Address sizes:                   48 bits physical, 48 bits virtual
CPU(s):                          16
On-line CPU(s) list:             0-15
Thread(s) per core:              2
Core(s) per socket:              8
Socket(s):                       1
NUMA node(s):                    1
Vendor ID:                       AuthenticAMD
CPU family:                      25
Model:                           1
Model name:                      AMD EPYC 7B13
Stepping:                        0
CPU MHz:                         2449.998
BogoMIPS:                        4899.99
Hypervisor vendor:               KVM
Virtualization type:             full
L1d cache:                       256 KiB
L1i cache:                       256 KiB
L2 cache:                        4 MiB
L3 cache:                        32 MiB
NUMA node0 CPU(s):               0-15
Vulnerability Itlb multihit:     Not affected
Vulnerability L1tf:              Not affected
Vulnerability Mds:               Not affected
Vulnerability Meltdown:          Not affected
Vulnerability Mmio stale data:   Not affected
Vulnerability Retbleed:          Not affected
Vulnerability Spec store bypass: Mitigation; Speculative Store Bypass disabled via prctl and seccomp
Vulnerability Spectre v1:        Mitigation; usercopy/swapgs barriers and __user pointer sanitization
Vulnerability Spectre v2:        Mitigation; Retpolines, IBPB conditional, IBRS_FW, STIBP conditional, RSB filling
Vulnerability Srbds:             Not affected
Vulnerability Tsx async abort:   Not affected
Flags:                           fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ht syscall nx mmxext fxsr_opt pdpe1gb rdtscp lm constant_tsc rep_good nopl nonstop_tsc cpuid extd_apicid tsc_known_freq pni pclmulqdq ssse3 fma cx16 pcid sse4_1 sse4_2 mov
                                 be popcnt aes xsave avx f16c rdrand hypervisor lahf_lm cmp_legacy cr8_legacy abm sse4a misalignsse 3dnowprefetch osvw topoext invpcid_single ssbd ibrs ibpb stibp vmmcall fsgsbase tsc_adjust bmi1 avx2 smep bmi2 erms invpcid rdseed adx smap clflushopt clwb sha_ni xsaveop
                                 t xsavec xgetbv1 clzero xsaveerptr arat npt nrip_save umip vaes vpclmulqdq rdpid fsrm
```
