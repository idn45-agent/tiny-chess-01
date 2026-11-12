# tiny-chess-01

A complete chess engine in a single Python file of at most 4,096 bytes,
written for the make.fun tiny-chess-01 competition. Standard library only.

Design: 0x88 board, snapshot make/unmake, negamax alpha-beta with a fixed
100,000-node budget (deterministic, no wall clock), quiescence search with
check evasions, check extensions, MVV-LVA move ordering, and an incremental
piece-square evaluation with the king table folded into the main table.

Run it with the competition environment URL set:

    MAKEFUN_ENV_URL=http://127.0.0.1:PORT python3 main.py
