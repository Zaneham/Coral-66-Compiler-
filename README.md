# CORAL 66 Compiler

> **Work in Progress** - Lexer and parser functional, codegen coming soon. The Queen's 1976 compiler had a head start.

A modern compiler for CORAL 66 (Computer On-line Real-time Applications Language), written in OCaml. Because someone has to keep the reactors running.

*The first email ever sent by a head of state was Queen Elizabeth II announcing a CORAL 66 compiler in 1976. If the Queen can do release announcements, so can we.*

## What is CORAL 66?

CORAL 66 was developed in 1964 at the Royal Radar Establishment in Malvern, because the Ministry of Defence looked at ALGOL and thought "this needs more real-time interrupt handling and fewer safety features." By 1971 it became the official programming language for British military systems, which is a polite way of saying the government mandated its use and everyone had to deal with it.

It's basically what happens when you ask British engineers to make a language for shooting things out of the sky and controlling nuclear reactors. Stiff upper lip, fixed-point arithmetic, and absolutely no dynamic memory allocation.

**Where it ran (and terrifyingly, still runs):**

| System | What It Does | Level of Concern |
|--------|--------------|------------------|
| RAF Ground Radar | Tracking aircraft over Britain | Moderate |
| Royal Navy Fire Control | Making ships go bang | High |
| Ferranti Argus 700 | Nuclear reactor control at Torness and Heysham 2 | Existential |
| System X Exchanges | British Telecom's telephone network | "Hello? HELLO?" |
| Tornado GR4 | Ground support systems | Gulf War tested |

Some of these systems have been running since the 1970s. Someone has to maintain that code. This compiler exists because those people deserve better tools than whatever they're currently using.

## Features

This compiler does more than it probably should:

- **Full CORAL 66 Support** - Per the Official Definition published by Her Majesty's Stationery Office in 1970. Yes, HMSO published a programming language spec. Yes, it has a Crown Copyright notice.
- **Native Code Generation** - Via LLVM, because the Ferranti Argus 700 is hard to find these days
- **C Transpiler** - For when you need to see your reactor control logic in a language invented after 1960
- **Modern Diagnostics** - Source locations, helpful error messages, things the 70s didn't have
- **Static Analysis** - Catching bugs before they cause unplanned criticality events

## Building

```bash
# Install OCaml dependencies
opam install . --deps-only

# Build the compiler
dune build

# Compile some reactor control code
dune exec coral66c -- input.cor

# Question whether society was ready for this
```

### Requirements

- OCaml 4.14 or later
- Dune 3.0+
- Menhir (parser generator)
- A tolerance for languages that predate your parents
- Ideally no actual reactor to test this on

## Language Example

```coral
BEGIN
    COMMENT Reactor monitoring subsystem;
    COMMENT Ferranti Argus 700 - Torness Unit 1;

    INTEGER count, alarm_level := 0;
    FLOATING temperature, pressure;
    FIXED(16, 8) flow_rate;

    COMMENT Sensor array for core monitoring;
    FLOATING ARRAY readings[0:99];

    COMMENT Calculate running average;
    FLOATING PROCEDURE average(VALUE INTEGER: n);
    BEGIN
        INTEGER i;
        FLOATING sum := 0.0;
        FOR i := 0 STEP 1 UNTIL n DO
            sum := sum + readings[i];
        ANSWER sum / FLOATING(n)
    END;

    COMMENT Main monitoring loop;
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

Yes, `COMMENT` is the comment syntax. Yes, `ANSWER` returns values from procedures. Yes, `GOTO` is not only allowed but actively encouraged. Welcome to 1964.

## The Official Definition

This compiler implements the Official Definition of CORAL 66 as published by Her Majesty's Stationery Office in 1970. The document number is ISBN 0 11 470221 5 if you want to hunt down a copy. Good luck - most of them are in MOD archives or sitting in the offices of retired Ferranti engineers.

Key features from the spec:

| Feature | What It Means | Why It Exists |
|---------|---------------|---------------|
| `FIXED(m, n)` | Fixed-point with m total bits, n fractional | Floating point was expensive in 1964 |
| `TABLE` | Packed record structures | Memory was also expensive |
| `OVERLAY` | Manual memory management | Everything was expensive |
| `COMMON` | Shared memory between modules | Inter-process communication before IPC |
| `ABSOLUTE` | Put variables at specific addresses | Hardware register access |
| `CODE` | Inline assembly | When CORAL isn't low-level enough |

## Why OCaml?

Because writing a compiler for a British military language in a French academic language felt appropriately chaotic. Also menhir is genuinely excellent for parser generation and the type system catches bugs that would otherwise end up in reactor control code.

## Why Does This Exist?

1. Torness nuclear power station runs on 70+ Ferranti Argus 700 computers programmed in CORAL 66
2. Those systems are still operational
3. The engineers maintaining them deserve modern tooling
4. Historical preservation of British computing heritage
5. The LSP was already done and needed a friend
6. Someone on the internet said "you can't write a CORAL 66 compiler" and here we are

## Related Projects

If you've made it this far, you're clearly the sort of person who thinks "what if I learned a dead language" and means programming:

### The Vintage Language Collection

| Project | What It Is | Historical Significance |
|---------|-----------|------------------------|
| [jovial-compiler](https://github.com/Zaneham/jovial-compiler) | JOVIAL J73 native compiler | American answer to CORAL, for bombing things |
| [coral66-lsp](https://github.com/Zaneham/coral66-lsp) | Language Server for CORAL 66 | IntelliSense for reactor control |
| [hals-lsp](https://github.com/Zaneham/hals-lsp) | HAL/S Language Server | The Space Shuttle's programming language |
| [cms2-lsp](https://github.com/Zaneham/cms2-lsp) | CMS-2 Language Server | US Navy weapons systems |
| [chill-lsp](https://github.com/Zaneham/chill-lsp) | CHILL Language Server | ITU telecom switching |

### The Emulator Collection

| Project | What It Emulates | Why |
|---------|-----------------|-----|
| [voyager-fds-emulator](https://github.com/Zaneham/voyager-fds-emulator) | Voyager Flight Data Subsystem | It's still out there, transmitting |
| [minuteman-computer-emulator](https://github.com/Zaneham/minuteman-computer-emulator) | Minuteman missile guidance | Educational purposes only, obviously |
| [setun70-emulator](https://github.com/Zaneham/setun70-emulator) | Soviet trinary computer | Because binary is boring |

## Fun Facts

- **Queen Elizabeth II announced a CORAL 66 compiler in the first email ever sent by a head of state.** On 26 March 1976, Her Majesty visited the Royal Signals and Radar Establishment at Malvern and sent a message to all ARPANET users announcing compiler availability. Her username was `HME2`. Peter Kirstein set up her account and later recalled "all she had to do was press a couple of buttons." They stationed a young scientist by the mainframe in case the acoustic modem failed. It didn't. The Queen shipped a compiler release over the internet before most of your parents were born. Zero open issues. Mass closed as WONTFIX by Royal Prerogative.
- CORAL stands for Computer On-line Real-time Applications Language, which is a backronym that almost works
- In 1971 the British government said "you will write military software in CORAL 66" and that was that. Imagine your tech stack being mandated by the Ministry of Defence. Imagine the Jira tickets.
- The language was designed at the Royal Radar Establishment in Malvern - the same place that invented radar. They went from "detecting Nazi bombers" to "programming nuclear reactors" in about 20 years. Productivity kings.
- Ferranti, who made the Argus computers, also built the Manchester Mark 1 - the first stored-program computer. Same company went from "inventing modern computing" to "running British nuclear plants." No pressure.
- CORAL 66 is still classified as controlled technology in some contexts. You might need export licenses. For a programming language. From 1966.
- There are engineers alive today who have maintained the same CORAL 66 codebase for 40+ years. These people deserve statues.
- The Torness reactors are scheduled to run until 2030. The CORAL code will outlive most YC startups, your favourite JavaScript framework, and possibly democracy.
- The official spec was published by Her Majesty's Stationery Office with a Crown Copyright. This language has royal blood.

## Specification Documents

- Official Definition of CORAL 66 (HMSO 1970) - ISBN 0 11 470221 5
- DEF STAN 05-57 - Defence Standard for CORAL 66
- JSP 188 - Joint Service Publication on CORAL usage

If you work for the MOD and have access to these documents, I would be unreasonably grateful for copies.

## Contact

Found a bug? Want to discuss British military computing history? Need to tell someone that you maintain CORAL 66 code and have them actually understand what that means?

**zanehambly@gmail.com**

Based in New Zealand, where it's already tomorrow and the sheep outnumber the CORAL 66 programmers roughly eight million to one.

If you're maintaining reactor control systems at Torness or Heysham and stumbled upon this - hello! Your job is fascinating and I have questions.

## License

Apache 2.0

Because safety-critical tooling should be auditable, and anyone maintaining 50-year-old reactor control code has enough problems without license drama.

---

