# FAIRe term review for MIxS/DwC eDNA checklist proposal
This repository contains scripts and documentation for the TDWG/GSC eDNA task group to conduct a term-by-term review of the FAIRe checklist. 

# Background
The TDWG/GSC eDNA Task Group is working to develop an eDNA checklist under the DwC and MIxS standards.
To support this effort, a term-by-term review of the FAIRe checklist is being conducted to:
- Track the origin of each FAIRe term.
- Document how terms have been modified from their original source, e.g., MIxS, DwC, the DNA extension, and DwC-DP.
- Determine whether each term should be included in the proposed DwC/MIxS eDNA checklist.
To help visualise the decision-making process, an R function was developed to generate decision trees that capture and display the review outcomes for each term.

# General procedure for each FAIRe term
1. Create a GitHub issue for the term e.g., [Review: RecordedBy #245](https://github.com/FAIR-eDNA/FAIRe-GSC-TDWG-term-review/issues/245). <i>Note that in the modeling standard we are using, [LinkML](https://linkml.io/), 'terms' are referred to as 'slots'.</i>. (Steve - Done)
2. Raise disucssion points for the term (e.g., Miwa to generate the decision making tree for each term)
3. Add comments and feedback (TG members) 
4. Discuss and vote durign a meeting (e.g., whether to propose the term to GSC/TDWG)
7. Review SKOS mapping (Re-visit points raised in [Issue #14](https://github.com/FAIR-eDNA/FAIRe_checklist/issues/14))
8. Update issue status from "Review started" to "Review complete"

# General points to condiser for each term 
1. **Scope and inclusion :** In general, terms in the FAIRe checklist are there because they are identified as important/useful by the FAIRe WG. So  all FAIRe terms should be proposed to MIxS and DwC-DP <i>except</i> for those that fall out of the GSC/TDWG scope. For example, a) targeted-assay detection protocols without sequencing data for GSC, b) Unit terms for numeric variables (as GSC has their conventions for represneting unites)
2. **Modifications to existing MIxS/DwC terms :** Consider whether modifications made in the FAIRe checklists should be proposed. Note that checklist-specific examples may be accepted, but not descriptions (for now) by GSC.
3. **Controlled vocabularies and ontologies :** Where possible, terms should be controlled using existing ontologies. Note that some of the current FAIRe controlled terms may not yet aligned with established ontologies, hence require review.  
4. **Future FAIRev2 development :** (Probably once we complete above discussions and decisions for all exisitng FAIRe terms,) consider whether additional new terms should be proposed, both for FAIRe v2 and for the MIxS/DwC proposals.
