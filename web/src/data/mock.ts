// Static mock data only -- this app does not call the real SigRepo API.
// Field names mirror the real signature/collection/feature tables so the
// mockup reads like real repository content.

export interface Signature {
  signature_id: string;
  signature_name: string;
  user_name: string;
  visibility: "Public" | "Private";
  date_created: string;
  organism: string;
  assay_type: string;
  sample_type: string;
  phenotype: string;
  description: string;
  source_type: "local" | "remote";
  source_label: string;
}

export const signatures: Signature[] = [
  {
    signature_id: "sig_0001",
    signature_name: "LLFS_Aging_Gene_2023",
    user_name: "cvicnaire",
    visibility: "Public",
    date_created: "2023-11-02",
    organism: "Homo sapiens",
    assay_type: "transcriptomics",
    sample_type: "blood",
    phenotype: "aging",
    description: "Long Life Family Study aging-associated gene expression signature.",
    source_type: "local",
    source_label: "SigRepo",
  },
  {
    signature_id: "sig_0002",
    signature_name: "Aging_Proteome_Liver_Young_vs_Old",
    user_name: "Monti Lab",
    visibility: "Public",
    date_created: "2026-04-18",
    organism: "Homo sapiens",
    assay_type: "proteomics",
    sample_type: "liver",
    phenotype: "aging",
    description: "Public proteomics aging signature shared from a connected SigRepo node.",
    source_type: "remote",
    source_label: "Monti Lab Proteomics",
  },
  {
    signature_id: "sig_0003",
    signature_name: "GTEx_Heart_Left_Ventricle_Sex_Bias",
    user_name: "GTEx Consortium",
    visibility: "Public",
    date_created: "2026-03-29",
    organism: "Homo sapiens",
    assay_type: "transcriptomics",
    sample_type: "heart",
    phenotype: "sex bias",
    description: "Remote transcriptomic signature surfaced through the federated catalog.",
    source_type: "remote",
    source_label: "GTEx Public Atlas",
  },
  {
    signature_id: "sig_0004",
    signature_name: "Tumor_Secretome_Response_IFNg",
    user_name: "Monti Lab",
    visibility: "Public",
    date_created: "2026-05-01",
    organism: "Mus musculus",
    assay_type: "proteomics",
    sample_type: "tumor",
    phenotype: "immune response",
    description: "Example shared signature from an external node for source-aware browsing.",
    source_type: "remote",
    source_label: "Monti Lab Proteomics",
  },
  {
    signature_id: "sig_0005",
    signature_name: "Skeletal_Muscle_Sarcopenia_Signature",
    user_name: "jkim",
    visibility: "Private",
    date_created: "2026-01-14",
    organism: "Homo sapiens",
    assay_type: "transcriptomics",
    sample_type: "muscle",
    phenotype: "sarcopenia",
    description: "Age-related muscle wasting differential expression signature.",
    source_type: "local",
    source_label: "SigRepo",
  },
  {
    signature_id: "sig_0006",
    signature_name: "Kidney_Fibrosis_Metabolomics",
    user_name: "asantos",
    visibility: "Public",
    date_created: "2026-02-20",
    organism: "Mus musculus",
    assay_type: "metabolomics",
    sample_type: "kidney",
    phenotype: "fibrosis",
    description: "Metabolomic profiling of fibrotic vs. healthy kidney tissue.",
    source_type: "local",
    source_label: "SigRepo",
  },
];

export const featurePreview = [
  { feature_name: "ALB", symbol: "ALB", score: 3.42, direction: "+" },
  { feature_name: "APOA1", symbol: "APOA1", score: 2.87, direction: "+" },
  { feature_name: "SERPINA1", symbol: "SERPINA1", score: 2.66, direction: "+" },
  { feature_name: "HP", symbol: "HP", score: -2.11, direction: "-" },
  { feature_name: "TF", symbol: "TF", score: -2.45, direction: "-" },
];

export interface Collection {
  collection_id: string;
  collection_name: string;
  user_name: string;
  visibility: "Public" | "Private";
  date_created: string;
  num_signatures: number;
  description: string;
}

export const collections: Collection[] = [
  {
    collection_id: "col_0001",
    collection_name: "Aging Hallmarks Panel",
    user_name: "cvicnaire",
    visibility: "Public",
    date_created: "2026-01-05",
    num_signatures: 12,
    description: "Curated set of signatures spanning the classic hallmarks of aging.",
  },
  {
    collection_id: "col_0002",
    collection_name: "LLFS Multi-Omics Set",
    user_name: "jkim",
    visibility: "Public",
    date_created: "2025-11-22",
    num_signatures: 8,
    description: "Cross-assay signatures generated from the Long Life Family Study cohort.",
  },
  {
    collection_id: "col_0003",
    collection_name: "Draft: Sarcopenia Follow-up",
    user_name: "jkim",
    visibility: "Private",
    date_created: "2026-04-02",
    num_signatures: 3,
    description: "Work-in-progress collection for a follow-up muscle aging study.",
  },
];

export const referenceFeatures = [
  { feature_name: "TP53", symbol: "TP53", gene_id: "ENSG00000141510", chromosome: "17", assay_type: "transcriptomics" },
  { feature_name: "APOE", symbol: "APOE", gene_id: "ENSG00000130203", chromosome: "19", assay_type: "transcriptomics" },
  { feature_name: "IL6", symbol: "IL6", gene_id: "ENSG00000136244", chromosome: "7", assay_type: "transcriptomics" },
  { feature_name: "FOXO3", symbol: "FOXO3", gene_id: "ENSG00000118689", chromosome: "6", assay_type: "transcriptomics" },
  { feature_name: "SIRT1", symbol: "SIRT1", gene_id: "ENSG00000096717", chromosome: "10", assay_type: "transcriptomics" },
];

export const organismCounts = [
  { name: "Homo sapiens", value: 148 },
  { name: "Mus musculus", value: 62 },
  { name: "Rattus norvegicus", value: 14 },
];

export const assayCounts = [
  { name: "transcriptomics", value: 122 },
  { name: "proteomics", value: 54 },
  { name: "metabolomics", value: 28 },
  { name: "methylomics", value: 12 },
  { name: "snps", value: 8 },
];

export const topContributors = [
  { name: "Monti Lab", value: 64 },
  { name: "cvicnaire", value: 41 },
  { name: "jkim", value: 33 },
  { name: "GTEx Consortium", value: 27 },
  { name: "asantos", value: 19 },
];

export const repositorySummary = {
  total_signatures: 224,
  total_users: 18,
  total_organisms: 3,
  total_assays: 5,
};

export const connectedNodes = [
  { node_id: "monti_proteomics", node_name: "Monti Lab Proteomics", lab_name: "Monti Lab", status: "Connected", signatures: 148, last_sync: "2026-07-10 08:35" },
  { node_id: "gtex_public", node_name: "GTEx Public Atlas", lab_name: "GTEx Consortium", status: "Connected", signatures: 312, last_sync: "2026-07-10 08:41" },
];

export interface EnrichmentResult {
  geneset: string;
  pval: number;
  fdr: number;
  overlapCount: number;
  genesetSize: number;
}

export interface SignatureFeature {
  feature_name: string;
  score: number;
}

// Per-signature feature/score lists used by the Compare page. Feature name
// overlap across signatures is intentional -- it's what drives the shared
// vs. unique breakdown.
export const signatureFeatures: Record<string, SignatureFeature[]> = {
  sig_0001: [
    { feature_name: "ALB", score: 3.42 },
    { feature_name: "APOA1", score: 2.87 },
    { feature_name: "SERPINA1", score: 2.66 },
    { feature_name: "HP", score: -2.11 },
    { feature_name: "TF", score: -2.45 },
    { feature_name: "IL6", score: 1.82 },
    { feature_name: "CRP", score: 1.44 },
    { feature_name: "FOXO3", score: -1.65 },
  ],
  sig_0002: [
    { feature_name: "ALB", score: 2.95 },
    { feature_name: "APOA1", score: 2.31 },
    { feature_name: "TF", score: -1.98 },
    { feature_name: "SIRT1", score: -1.72 },
    { feature_name: "IGF1", score: 1.55 },
  ],
  sig_0003: [
    { feature_name: "NPPA", score: 2.61 },
    { feature_name: "MYH6", score: 1.89 },
    { feature_name: "IL6", score: 1.21 },
    { feature_name: "CRP", score: 0.98 },
    { feature_name: "TNNT2", score: -2.05 },
  ],
  sig_0004: [
    { feature_name: "IFNG", score: 3.11 },
    { feature_name: "CXCL9", score: 2.74 },
    { feature_name: "IL6", score: 1.9 },
    { feature_name: "PDCD1", score: 1.35 },
  ],
  sig_0005: [
    { feature_name: "ALB", score: 2.9 },
    { feature_name: "APOA1", score: 2.1 },
    { feature_name: "MYOD1", score: 3.3 },
    { feature_name: "ACTN3", score: -1.9 },
    { feature_name: "IL6", score: 2.2 },
    { feature_name: "TNF", score: 1.6 },
  ],
  sig_0006: [
    { feature_name: "COL1A1", score: 2.4 },
    { feature_name: "TGFB1", score: 2.1 },
    { feature_name: "FOXO3", score: -1.4 },
    { feature_name: "TNF", score: 1.3 },
  ],
};

export const enrichmentResults: EnrichmentResult[] = [
  { geneset: "REACTOME_CELLULAR_SENESCENCE", pval: 0.00012, fdr: 0.0021, overlapCount: 14, genesetSize: 86 },
  { geneset: "KEGG_COMPLEMENT_AND_COAGULATION_CASCADES", pval: 0.00045, fdr: 0.0038, overlapCount: 9, genesetSize: 68 },
  { geneset: "WP_OXIDATIVE_STRESS_RESPONSE", pval: 0.0012, fdr: 0.0091, overlapCount: 7, genesetSize: 41 },
  { geneset: "REACTOME_INNATE_IMMUNE_SYSTEM", pval: 0.0034, fdr: 0.018, overlapCount: 11, genesetSize: 122 },
  { geneset: "KEGG_PPAR_SIGNALING_PATHWAY", pval: 0.0067, fdr: 0.031, overlapCount: 5, genesetSize: 29 },
  { geneset: "REACTOME_EXTRACELLULAR_MATRIX_ORGANIZATION", pval: 0.0091, fdr: 0.038, overlapCount: 8, genesetSize: 94 },
];
