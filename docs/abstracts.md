# Conference Abstracts – Clinical TFL Viewer v3.1

## Abstract 1: CDISC Standards and AI Integration — Learnings from Hackathons and Ideas to Shape Future Innovation

**Title:** Building a CDISC ARS-Compliant Interactive TFL Viewer with AI-Assisted Development in R

**Background:**
The CDISC Analysis Results Standard (ARS) provides a machine-readable framework for defining and tracing clinical trial outputs — tables, figures, and listings (TFLs) — from their source data through to final display. While the standard holds significant promise for reproducibility and regulatory traceability, practical open-source implementations that bring ARS to life in an interactive setting remain limited. At the same time, AI-assisted coding tools are increasingly available to statistical programmers, yet their role in building standards-compliant clinical trial applications has not been widely explored.

**Objectives:**
This work presents the Clinical TFL Viewer v3.1, an interactive R/Shiny application that implements the CDISC ARS model for TFL generation, filtering, and review. We describe both the technical design — how ARS metadata structures (analysis sets, methods, data subsets, and display definitions) are encoded and rendered — and the development process, in which an AI coding assistant was used collaboratively to build the application, including its review audit trail and ARS JSON export functionality.

**Methods:**
The application was developed in R using Shiny and bslib (Bootstrap 5). Each TFL script defines a structured metadata contract aligned with ARS concepts: analysis sets with population-level filters, analysis definitions with grouping variables and method references, and display metadata including titles and footnotes. The app dynamically executes TFL scripts against synthetic CDISC ADaM datasets (ADSL, ADAE, ADLB, ADTTE), applies user-driven filters, and exports machine-readable ARS JSON. A role-based review system with threaded comments, audit status tracking, and Excel export was developed iteratively using AI pair-programming to accelerate design, debug serialization issues, and implement features from specification to working code.

**Results:**
The resulting application demonstrates that the ARS model can be practically implemented in an open-source interactive tool, enabling end-to-end traceability from ADaM source data to rendered output and machine-readable metadata. The AI-assisted development approach reduced implementation time for the review audit trail and allowed rapid iteration on design decisions, while the developer retained full control over architectural choices and standards compliance. Key challenges included JSON serialization fidelity for threaded review data and mapping ARS concepts to an interactive filtering paradigm.

**Conclusions:**
Implementing CDISC ARS in an interactive R/Shiny application is feasible and yields a tool with practical value for TFL review and traceability. AI-assisted development can meaningfully accelerate the building of standards-compliant tools when the developer provides domain expertise and architectural direction. This work suggests a model for future innovation in which AI and CDISC standards are complementary — AI lowers the barrier to building standards-based tooling, and standards provide the structure that keeps AI-generated code clinically meaningful.

---

## Abstract 2: New Ways of Working That Simplify Processes and Improve Quality

**Title:** An Interactive TFL Review Workflow Using R/Shiny with Role-Based Audit Trails

**Background:**
The review of tables, figures, and listings (TFLs) in clinical trials typically involves circulating static outputs — PDFs, RTF documents, or Excel files — among reviewers from multiple functions including statistics, safety, medical, programming, and clinical operations. This process is fragmented: review comments are captured in separate emails, trackers, or annotated documents, making it difficult to maintain a clear audit trail of who reviewed what, when, and whether issues were resolved. As study complexity grows, so does the burden of coordinating and documenting this review process.

**Objectives:**
This work presents an interactive R/Shiny application that integrates TFL generation, dynamic data exploration, and a structured review workflow into a single tool. The objective is to simplify the TFL review process by replacing static output circulation with an interactive environment where reviewers can examine outputs alongside their source data, submit role-tagged comments, and track review completion across all TFLs in a study.

**Methods:**
The Clinical TFL Viewer v3.1 was developed using R, Shiny, and bslib. It dynamically executes TFL scripts against CDISC ADaM datasets and presents outputs in an interactive viewer with tabbed access to the rendered output, filtered source data, dataset exploration tools, and the underlying code. A review system allows users to identify themselves by name and role (from predefined roles including Statistics Lead, Safety Lead, Medical Lead, Programming Lead, Clinical Operations Lead, and Trial Programmer), submit comments on specific TFLs, and reply to existing comments in threaded discussions. An audit status bar displays which reviewer roles have provided input for each TFL, and a downloadable Excel workbook compiles all review comments and a cross-TFL audit summary for study documentation.

**Results:**
The application consolidates TFL output review, data investigation, and audit documentation into a single interactive session. Reviewers can verify outputs against source data in real time rather than relying on static snapshots. The threaded comment system with role-based tagging provides clear attribution and supports structured dialogue between reviewers and programmers. The Excel audit export produces a ready-made review log suitable for inclusion in study documentation, with columns for TFL identification, threading, commenter role, timestamp, and response attribution. The role-based audit status bar gives an immediate visual summary of review coverage across functional areas.

**Conclusions:**
Integrating TFL review into an interactive application reduces the fragmentation inherent in static output review workflows. By combining output viewing, data exploration, and structured commenting in one tool, the application simplifies coordination across review functions and produces a more complete and traceable audit trail. This approach demonstrates how R/Shiny can be used not only for statistical output generation but also for streamlining the collaborative processes that surround it.
