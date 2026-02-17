# CORAL 66 Compiler

> **Work in Progress** - Lexer and parser functional, codegen not yet implemented.

A compiler for CORAL 66 (Computer On-line Real-time Applications Language), written in OCaml.

## What is CORAL 66?

CORAL 66 was developed in 1964 at the Royal Radar Establishment in Malvern. By 1971 it became the mandated programming language for British military systems. It powered RAF radar, Royal Navy fire control, the Tornado GR4 ground systems, British Telecom's System X exchanges, and — most notably — the Ferranti Argus 700 computers controlling nuclear reactors at Torness and Heysham 2. Some of these systems have been running since the 1970s.

## Current Status

- **Lexer** (ocamllex) - complete
- **Parser** (menhir LR(1)) - complete
- **Semantic analysis** - not yet implemented
- **Code generation** - not yet implemented

## Building

```bash
opam install . --deps-only
dune build
```

### Requirements

- OCaml 4.14+
- Dune 3.0+
- Menhir

## Language Example

```coral
BEGIN
    INTEGER count, alarm_level := 0;
    FLOATING temperature;
    FIXED(16, 8) flow_rate;
    FLOATING ARRAY readings[0:99];

    FLOATING PROCEDURE average(VALUE INTEGER: n);
    BEGIN
        INTEGER i;
        FLOATING sum := 0.0;
        FOR i := 0 STEP 1 UNTIL n DO
            sum := sum + readings[i];
        ANSWER sum / FLOATING(n)
    END;

    temperature := average(100);
    IF temperature > 500.0 THEN
    BEGIN
        alarm_level := 1;
        GOTO emergency
    END;

emergency:
    COMMENT This bit runs the sirens
END
```

## The Official Definition

Based on the Official Definition of CORAL 66 (HMSO, 1970), ISBN 0 11 470221 5.

Key language features:

| Feature | Description |
|---------|-------------|
| `FIXED(m, n)` | Fixed-point with m total bits, n fractional |
| `TABLE` | Packed record structures |
| `OVERLAY` | Manual memory management |
| `COMMON` | Shared memory between modules |
| `ABSOLUTE` | Variables at specific hardware addresses |
| `CODE` | Inline assembly |

## Specification Documents

- Official Definition of CORAL 66 (HMSO 1970) - ISBN 0 11 470221 5
- DEF STAN 05-57 - Defence Standard for CORAL 66
- JSP 188 - Joint Service Publication on CORAL usage

## Related Projects

- [coral66-lsp](https://github.com/Zaneham/coral66-lsp) - Language Server for CORAL 66
- [jovial-compiler](https://github.com/Zaneham/jovial-compiler) - JOVIAL J73 compiler
- [hal-s-compiler](../hal-s-compiler) - HAL/S compiler

## License

Apache 2.0
