Use NAMESPACE.proposed as the complete namespace for this implementation.
DESCRIPTION requires Imports: infercalcR, utils and Suggests: ggplot2 (>= 3.4.0), DescrTab2.
Existing stats import is unnecessary for production reporting (used in tests only; base R).
No contributed dependency beyond infercalcR is mandatory. DescrTab2 is pinned to 2.1.26
at upstream commit f157380e7abcb7964612a480e09e59a45e7a01be, archive SHA256
354157ec6b7ceef2cb328395e7fd20b0f13dff7715525284c5deacd2ba076f27.
Do not install archived DescrTab2 from an unpinned latest-source URL in CI.
