from pathlib import Path
import re
from copy import deepcopy

from docx import Document
from docx.shared import Inches, Pt, RGBColor
from docx.enum.text import WD_BREAK
from docx.oxml.ns import qn

ROOT = Path(__file__).parent
SRC = ROOT / 'upload/Brazilian_coffee_manuscript_submission_ready(1).docx'
SUPP = ROOT / 'coffee_sources/Brazilian_coffee_supplementary_submission_ready.docx'
OUT = ROOT / 'coffee_revision'
OUT.mkdir(exist_ok=True)

def set_par(p, text):
    # Preserve the paragraph's style and anchor while replacing its editable text.
    for run in p.runs:
        run.text = ''
    p.add_run(text)

def delete_par(p):
    p._element.getparent().remove(p._element)

def sanitize_title(document):
    for node in (document.styles['Title']._element, document.paragraphs[0]._element):
        for b in node.xpath('.//w:pBdr'):
            b.getparent().remove(b)
    document.styles['Title'].font.color.rgb = RGBColor(0,0,0)
    for r in document.paragraphs[0].runs:
        r.font.color.rgb = RGBColor(0,0,0)

d = Document(SRC)
ps = d.paragraphs[:]
ed = {
0: 'Contrasting adjustments sustain Brazilian coffee production amid rising climatic exposure',
6: 'Canephora output rose 43.6% as area fell, driven by within-municipality productivity.',
7: 'Arabica output was nearly stable, with positive contributions from both redistribution and productivity.',
8: 'Fixed production geographies warmed and experienced more hot days from 1981 to 2024.',
9: 'A standardized rainfed balance identified worsening antecedent dry-season water supply.',
10: 'Municipal redistribution toward cooler settings did not establish Arabica field migration.',
12: ('National crop output can conceal changes in production geography and intensification as climatic exposure rises. We examined this possibility for Brazilian Arabica and Canephora coffee using species-resolved municipal statistics (2012–2024), NASA POWER climate histories (1981–2024), a sequential rainfed water balance, an exact decomposition of production change, and spatial and statistical sensitivity analyses. Comparing 2012–2015 with 2021–2024, Arabica production rose 2.3% despite a 5.3% decline in harvested area; geographic redistribution and within-municipality productivity contributed +4.2 and +3.7 percentage points to production change. Canephora output rose 43.6% as harvested area fell 15.9%, with a +62.7-point productivity contribution. At fixed baseline production weights, mean temperature rose by 0.39 and 0.32 °C decade−1, and days with maximum temperature ≥32 °C rose by 8.2 and 14.7 days decade−1, for Arabica and Canephora respectively. Simulated water satisfaction declined most consistently in April–August before the production year. Municipal shifts placed more Arabica area in municipalities with higher reference elevation, but an independent coffee-pixel sensitivity showed no corresponding upslope signal. Hydroclimatic variables explained some within-sample productivity variation while adding little chronological prediction skill. These results show that aggregate output can be maintained by distinct internal adjustments during worsening exposure; they do not quantify the causal climate cost of production or prove that these adjustments will persist.'),
13: 'Keywords: Coffea arabica; Coffea canephora; production geography; climate exposure; water balance; intensification; temporal validation',
15: ('A crop can maintain output while the conditions under which it is grown deteriorate. This is especially consequential for perennial coffee: a productive stand reflects past planting, cultivar and management decisions, and a damaging season can affect later harvests. Brazil offers a national test of this problem because it produces both Arabica (Coffea arabica) and Canephora (Coffea canephora) across different environments and production systems. National production alone cannot reveal whether an apparent capacity to cope with climate pressure rests on higher productivity, a different production geography, or both.'),
16: ('Heat and water availability affect coffee growth, flowering and fruit development (DaMatta and Ramalho, 2006; Camargo and Camargo, 2001). Evidence of carryover after temperature shocks highlights the need to consider more than a single harvest (Rising, 2024). Yet the future of Brazilian coffee remains uncertain: suitability, process-based crop and econometric models target different outcomes and represent management and adjustment differently (Bunn et al., 2015; Gomes et al., 2020; Freitas et al., 2024; Della Peruta et al., 2025; Xiao et al., 2025). Observed output and experienced climate must therefore be examined together, with their distinct interpretations kept clear.'),
17: ('The main gap is empirical. Most assessments project future suitability or yield, while few separate realized changes in national output into harvested-area contraction, productivity within existing municipalities, and redistribution among them. A municipal shift toward cooler reference environments might also be mistaken for movement of coffee fields upslope. Annual rainfall totals can miss seasonal water shortages, and correlations between weather and realized productivity need not improve forecasts for subsequent years. These are different questions requiring separate estimands.'),
18: ('We combine Brazilian municipal production for 2012–2024 with daily climate histories, a standardized sequential water balance and an exact production-change decomposition. Fixed 2012–2015 production weights isolate temporal change in exposure; fixed 1981–2010 climate isolates the environmental composition of geographic redistribution. An independent coffee-pixel sensitivity tests the limits of municipal inference. Finally, panel associations, chronological holdouts and two descriptive shock episodes probe the link between hydroclimate and production without claiming causal attribution.'),
19: ('We ask whether Arabica and Canephora sustained output through the same adjustment pathway, whether the climates experienced by fixed production geographies became more stressful, and how far municipal data can take us in interpreting geographic adjustment and temporal prediction. Keeping those questions separate makes it possible to read apparent resilience as a measured production outcome rather than an assumed absence of climate risk.'),
30: ('Daily climate histories came from NASA POWER meteorological and solar products (NASA POWER, 2026). Series were downloaded once per unique source cell and linked to 1,876 municipal reference points; the workflow used 558 meteorological cells and 286 radiation cells per solar epoch. Municipalities sharing a source cell do not provide independent meteorological observations. The reference point is a proxy for the municipality, not the location or microclimate of a coffee field.'),
31: ('We derived annual mean temperature, days with maximum temperature ≥32 °C, precipitation and longest dry sequence. Daily reference evapotranspiration (ET0) followed the project implementation of FAO Penman–Monteith (Allen et al., 1998). Recent contrasts compare 2021–2024 with 2012–2015. To separate temporal climate exposure from spatial redistribution, national series were weighted by each species’ mean harvested area in 2012–2015 and kept fixed over time. Theil–Sen slopes and Mann–Kendall diagnostics summarize 1981–2024 trends for variables with comparable histories; the water balance uses its separately documented 2001–2024 span. Neither a four-year contrast nor a trend is a causal attribution of anthropogenic climate change.'),
40: ('For 2012–2024 we modelled log municipal productivity using municipality intercepts, municipality-specific linear trends and common year effects. A sensitivity model added municipality-specific odd–even terms to account for alternating output patterns without assuming observed biological biennial phase. Predictors were fitted across four prespecified calendar windows. We compared heat, heat with precipitation and heat with simulated water satisfaction; the headline water specification used CAD = 100 mm and Kc = 0.95.'),
41: ('Covariance estimation clustered by meteorological cell, radiation cell and reporting year when estimable. With only 13 annual outcomes, intervals are exploratory and multiple windows increase the risk of chance findings. Coefficients are conditional associations with reported productivity. They cannot identify damage attributable to climate, a physiological response, or the management change underlying the productivity contribution in the decomposition.'),
43: ('Chronological validation held out each year from 2020 through 2024, fitting each model only to earlier production outcomes and excluding unavailable test-year fixed effects. The comparator used local historical behavior without climate; alternatives added heat, precipitation or water satisfaction. We assessed common held-out records with log-productivity error and squared-error skill relative to the comparator. Weather histories and analytical eligibility were assembled retrospectively; the exercise tests temporal transfer of fitted associations, not an operational forecast with a specified lead time.'),
50: ('Between 2012–2015 and 2021–2024, Arabica production rose 2.35% as harvested area contracted 5.3% and aggregate productivity rose 8.1%. Canephora production rose 43.56% despite a 15.9% area contraction, alongside a 70.8% rise in aggregate productivity (Fig. 1a–c). Four-year means reduce the influence of any one season but do not eliminate biennial variation.'),
51: ('The accounting decomposition exposes different pathways (Fig. 1d; Supplementary Table S3). For Arabica, total area contributed −5.12 percentage points to production change, redistribution +4.17, productivity within continuing municipalities +3.67, entry +0.09 and exit −0.47. For Canephora, the corresponding contributions were −21.57, +2.39, +62.66, +0.38 and −0.29 points. Values sum to the observed changes subject to rounding. The productivity term includes all unobserved influences that changed municipal output per harvested hectare; it is not a measured technological effect.'),
55: ('The Arabica production belt remained concentrated in southeastern and southern Brazil, but harvested-area shares shifted among municipalities within it (Fig. 2a–c). Canephora shares changed more unevenly in Rondônia and the Espírito Santo–Bahia region (Fig. 2d–f). The maps show changes among administrative reporting units; they cannot track farm moves or identify their drivers.'),
56: ('Offsetting gains and losses make a single national center a poor summary of this redistribution. Supplementary Figure S4 reports production- and area-weighted center and concentration diagnostics; the share changes are the spatial quantity used in the decomposition.'),
60: ('At fixed 2012–2015 area weights, mean temperature in the two four-year periods increased by 0.47 °C for Arabica and 0.58 °C for Canephora. Annual days with maximum temperature ≥32 °C increased by 15.3 and 25.6 days, respectively. Period differences in precipitation were spatially mixed (Fig. 3a–c; Supplementary Fig. S5). These are changes in gridded climate at representative municipalities, not observations at individual farms.'),
61: ('The long-term fixed-geography series showed warming of 0.39 °C decade−1 for Arabica and 0.32 °C decade−1 for Canephora during 1981–2024. Hot-day exposure increased by 8.18 and 14.71 days decade−1, while annual precipitation declined by approximately 101 and 90 mm decade−1 (Supplementary Fig. S2 and Table S4). The longest annual dry sequence had no robust directional trend under the same diagnostic. Annual rainfall and within-season water availability therefore need separate treatment.'),
65: ('Seasonal simulated rainfed water satisfaction changed differently across the production calendar (Fig. 3d; Supplementary Fig. S6). In the central scenario, the April–August window preceding production year t (W2) fell by 11.0 percentage points for Arabica and 8.4 points for Canephora. Canephora W3 fell by 6.8 points. W4 increased by about 9.4 points for both species, illustrating why annual precipitation cannot represent the timing of supply and demand.'),
66: ('W2 declines were negative under all 12 CAD and Kc scenarios. The scenario envelope expresses parameter sensitivity, not sampling uncertainty. These ratios describe a standardized system without irrigation; they do not estimate water deficits experienced by irrigated farms or equivalent losses of harvested output.'),
68: ('Changing harvested-area weights placed Arabica in municipalities with reference elevations about 26 m higher and Canephora about 20 m higher. At a fixed 1981–2010 municipal climatology, those weights shifted Arabica toward a mean environment 0.10 °C cooler and 3.8 fewer hot days annually; Canephora shifted toward a mean environment 0.28 °C cooler and 5.1 fewer hot days (Fig. 4a–c). This is compositional environmental redistribution among municipalities.'),
69: ('Persistent MapBiomas coffee pixels did not reproduce an Arabica upslope signal: average mapped elevation changed by −1.6 m under both species-allocation rules. For Canephora, the estimated change was +25.0 m in dominant-species municipalities but −6.3 m with proportional allocation (Fig. 4d; Supplementary Table S7). Changing mapped coverage and uncertain species identity prevent a definitive field-migration estimate. In particular, the Arabica municipal shift cannot be cited as evidence that fields moved upslope.'),
73: ('In the odd–even sensitivity model, a 0.10 lower ETr/ETc in W2 and W3 was conditionally associated with 4.67% and 5.38% lower Canephora productivity; exploratory intervals excluded zero. The corresponding Arabica W2 estimate had the opposite sign (+1.48%), and its other water estimates had intervals crossing zero (Fig. 5b). A fixed calendar window, especially one shared across species, does not define a biological response.'),
74: ('For Canephora, heat plus water balance explained 5.31% of variation remaining after local controls within sample, versus 2.15% for heat plus precipitation and 1.19% for heat alone. The equivalent Arabica figures were 1.39%, 1.42% and 1.09%. In pooled 2020–2024 holdouts, the water model reduced squared error by only 1.48% relative to the no-climate comparator for Arabica and increased it by 2.94% for Canephora (Fig. 5c,d). Conditional explanation and temporal prediction therefore gave different assessments of value.'),
78: ('The two event-aligned diagnostics are consistent with production impacts extending beyond the weather anomaly, although other concurrent factors cannot be excluded (Supplementary Figs. S8–S10). Across Minas Gerais, São Paulo and Paraná, April–September precipitation fell 121 mm below its reference in 2020 and 156 mm below in 2021. In June–July 2021, municipalities representing 15.2% of fixed Arabica area had a minimum temperature below 2 °C. Arabica production in the following positive-cycle year, 2022, was 84.1% of the same-parity 2012–2019 baseline; productivity was 85.4%.'),
79: ('In Espírito Santo, Canephora-area-weighted precipitation was below its 1981–2010 reference by 389 mm in 2014, 553 mm in 2015 and 271 mm in 2016, with hot-day anomalies of +113 and +88 days in 2015 and 2016. Production fell to 56.4% and productivity to 60.9% of the 2012–2013 baseline in 2016. Production was still below baseline in 2017 and rose above it in 2018. These descriptive sequences motivate explicit study of recovery, but do not isolate the yield effect of individual hazards.'),
77: '3.7 Recent adverse seasons were followed by multi-year production shortfalls',
81: '4.1 Different pathways under the same national climate pressure',
82: ('The decomposition changes the interpretation of Brazilian coffee’s apparent resilience. Canephora’s 43.6% production increase occurred during area contraction and was overwhelmingly associated with higher productivity within continuing municipalities. Arabica’s 2.3% increase masked similarly sized positive contributions from redistribution and within-municipality productivity against shrinking area. The accounting categories describe where observed production change came from. Neither higher productivity nor a municipal shift can be equated with successful climate adaptation without information on varieties, irrigation, orchard renewal, prices and decisions at farm scale.'),
83: ('These adjustments occurred as fixed geographies warmed and accumulated more hot days. The contrast is the study’s central finding: the same aggregate output trajectory can coexist with intensifying exposure and very different internal reorganization. Process-based projections find substantial scope for management to alter future Arabica outcomes, while also showing spatially heterogeneous risks (Freitas et al., 2024; Della Peruta et al., 2025). Our historical observations establish neither the avoided loss from adjustment nor how long the adjustments can continue.'),
84: '4.2 Municipal redistribution and field movement are distinct',
85: ('Arabica area became more concentrated in municipalities represented by higher and cooler reference environments, but mapped coffee-pixel elevation was virtually unchanged in the sensitivity analysis. Administrative redistribution thus has an environmental meaning without establishing recent upslope relocation of farms. The opposite Canephora pixel results across species-allocation rules underscore how much inference depends on imperfect crop identification. Better field boundaries and repeated species-specific masks are required to test actual movement.'),
86: ('These distinctions matter when comparing observations with future suitability maps. Suitability identifies possible environments under stated assumptions, while production weights record a response shaped by land, capital, infrastructure and management. A 0.10 °C compositional cooling of Arabica’s municipal environment is small relative to long-term temporal warming in the fixed geography; it should not be described as fully offsetting climatic pressure (Bunn et al., 2015; Dias et al., 2024).'),
87: '4.3 Seasonal water supply improves explanation more than prediction',
88: ('The seasonal balance revealed a consistent loss of rainfed water satisfaction in W2, even though W4 improved. The direction of W2 survived 12 combinations of water-holding capacity and crop coefficient, but the magnitude remains scenario dependent. Fixed windows and absent irrigation data are consequential: actual root-zone water supply, crop phenology and management vary among municipalities and species. The unexpected positive Arabica W2 association is a warning against converting model coefficients into a simple physiological damage function.'),
89: ('Within-sample Canephora associations gained information from the water balance that precipitation alone missed, yet the same model performed worse than local history when transferred chronologically. Management changes, reporting error, biennial dynamics and limited annual replication may all weaken that transfer. Climate indices can still characterize exposure, but a useful seasonal forecast would require truly available predictors at a defined lead time and independent data on orchard state, cultivar and water management.'),
90: '4.4 Perennial recovery and the limits of inference',
91: ('The Arabica drought–frost and Espírito Santo drought episodes show production remaining depressed after or across adverse seasons. This is plausible for a perennial crop whose productive structure persists across harvests, and independent work finds multiyear effects of extreme temperature in Brazilian coffee (Rising, 2024). Our episodes are descriptive: calendar parity, changing area, management and other contemporaneous influences preclude attributing the observed shortfall to the selected weather events. Repeated hazards and the time available for recovery merit direct field-level study.'),
92: ('The principal data limitation is scale. IBGE statistics aggregate farms and do not harmonize all historical municipal boundaries. NASA POWER cells share observations among municipalities and cannot resolve mountain microclimates. The rainfed water balance fixes Kc and CAD scenarios without observing soil, irrigation or crop stage. MapBiomas class 46 combines species and its coverage of IBGE area changed between the periods. Only 13 annual production outcomes support the panel analysis. These limitations restrict causal claims, fine-scale migration inference and forecast use, while preserving the value of the separate descriptive estimands.'),
93: '4.5 Implications for system monitoring',
94: ('Monitoring adaptation in perennial crops requires more than tracking national output. Harvested area, within-location productivity, changes among production locations, temporal climate exposure and recovery after shocks should be reported separately. In Brazil these indicators tell a coherent but conditional story: production persisted through divergent species-specific adjustments as the climates represented by fixed coffee geographies became hotter and drier in important dimensions. Whether those adjustments remain feasible is an open question that needs field-scale crop, management and economic data.'),
110: ('Brazilian Arabica and Canephora maintained or increased production from 2012–2015 to 2021–2024 through different combinations of area loss, municipal redistribution and productivity growth. The fixed production geographies simultaneously warmed, experienced more hot days and showed an antecedent seasonal decline in standardized rainfed water supply. Arabica’s shift toward cooler municipal reference environments did not establish upslope movement of fields. Hydroclimatic associations had limited chronological predictive value. National output is therefore an incomplete measure of resilience: the processes sustaining it, their costs and their durability remain the central questions.'),
112: ('The IBGE PAM, NASA POWER and MapBiomas source data are publicly available from the cited repositories. The processed analysis datasets, code and versioned source extracts have not yet been assigned a public repository DOI; this statement must be replaced with the final deposit link before submission.'),
114: ('Analysis was scripted in R. A public repository containing extraction details, production and climate audits, water balance, decomposition, spatial analyses and model code is required before submission; the persistent link must be inserted here.'),
118: ('Competing interests declaration to be confirmed by every author before submission.'),
}
for i,t in ed.items():set_par(ps[i],t)

# Remove duplicated digressions and superseded discussion/conclusion material.
for i in range(95,109):
    if i in ed: continue
    delete_par(ps[i])

# Replace four existing chart images with precisely redrawn panels; preserve maps.
figures = OUT / 'figures'
for index, name in [(0,'Figure1_adjustment_pathways.png'),(2,'Figure3_climate_pressure.png'),(3,'Figure4_environmental_redistribution.png'),(4,'Figure5_water_productivity.png')]:
    shape = d.inline_shapes[index]
    shape._inline.graphic.graphicData.pic.blipFill.blip.embed
    rid = shape._inline.graphic.graphicData.pic.blipFill.blip.embed
    part = d.part.related_parts[rid]
    part._blob = (figures/name).read_bytes()

set_par(ps[53], 'Figure 1. Divergent production pathways for Brazilian Arabica and Canephora. (a–c) Annual national production, harvested area and aggregate productivity in 2012–2024. (d) Exact Shapley contributions to the change between the means of 2012–2015 and 2021–2024, expressed as percentage points of baseline production. Negative and positive bars are accounting contributions; the observed changes are +2.35% and +43.56%, respectively. Entry and exit refer to municipalities transitioning into or out of positive harvested area.')
set_par(ps[63], 'Figure 3. Changing climate exposure and seasonal water supply at Brazilian coffee locations. (a–c) Municipal changes in mean temperature, days with Tmax ≥32 °C and annual precipitation between 2012–2015 and 2021–2024. (d) Change in ETr/ETc in the central standardized rainfed scenario, shown in percentage points for four windows relative to production year t (definitions in Supplementary Table S6). The 12-scenario parameter sensitivity is reported in Supplementary Figure S6; ETr/ETc is not observed farm water use.')
set_par(ps[71], 'Figure 4. Geographic redistribution at municipal and mapped-coffee scales. (a,b) Harvested-area distributions by municipal reference-elevation band. (c) Changes in area-weighted municipal reference elevation, with shifts in the fixed 1981–2010 climatology stated in the text. (d) Change in mean elevation of persistent MapBiomas coffee pixels under dominant-species municipality and proportional-allocation sensitivities. Municipal reference elevations are not field elevations; the mapped coffee class does not distinguish species and temporal coverage changed.')
set_par(ps[76], 'Figure 5. Seasonal water supply, conditional productivity associations and temporal validation. (a) Central-scenario change in ETr/ETc between comparison periods. (b) Conditional log-productivity associations from the odd–even sensitivity model; circles denote +100 mm precipitation and triangles denote −0.10 ETr/ETc, with exploratory 95% intervals. (c) Pooled 2020–2024 chronological squared-error improvement versus a no-climate comparator (positive is better). (d) Partial variance explained within sample beyond local controls. These quantities do not estimate climate-attributable damage or operational forecast skill.')

# Add targeted, verified Agricultural Systems citation and prune unused local news sources.
ref = 'Della Peruta, R., Mereu, V., Spano, D., Marras, S., Vezy, R., Trabucco, A., 2025. Projecting trends of arabica coffee yield under climate change: A process-based modelling study at continental scale. Agricultural Systems 227, 104353. https://doi.org/10.1016/j.agsy.2025.104353.'
ps[127]._element.addnext(deepcopy(ps[127]._element))
from docx.text.paragraph import Paragraph
pnew = Paragraph(ps[127]._element.getnext(), ps[127]._parent)
set_par(pnew, ref)
for p in list(d.paragraphs):
    if p.text.startswith(('IDR-Paraná, 2025.','SEAB-Paraná, 2025.','Morelato, R.T., 2018.',
                          'Assad, E.D.,','Brito, T.T.,','Coltri, P.P.,','Conab, 2022.',
                          'Ferreira, W.P.M.,','Kath, J.,','USDA Foreign Agricultural Service,',
                          'Zaro, G.C.,','Dias, C.G., Martins, F.B., Martins, M.A., Tomasella, J.,')):
        delete_par(p)

for p in d.paragraphs:
    if p.text.startswith('IBGE, 2026. Produção Agrícola Municipal'):
        set_par(p,'IBGE, 2026. Produção Agrícola Municipal, SIDRA Table 5457. Instituto Brasileiro de Geografia e Estatística, Rio de Janeiro. https://sidra.ibge.gov.br/pesquisa/pam/tabelas/.')
    if p.text.startswith('NASA POWER, 2026. Prediction'):
        set_par(p,'NASA POWER, 2026. POWER Data Methodology and Daily Agroclimatology Data Services. NASA Langley Research Center. https://power.larc.nasa.gov/docs/methodology/.')

sanitize_title(d)

for section in d.sections:
    section.header_distance = Inches(0.36)
    section.footer_distance = Inches(0.38)

for p in list(d.paragraphs):
    if p._element.xpath('.//w:drawing'):
        p.paragraph_format.keep_with_next = True
    if p.style.name == 'Figure Caption':
        p.paragraph_format.keep_together = True
    if not p.text.strip() and not p._element.xpath('.//w:drawing') and not p._element.xpath('.//w:br'):
        delete_par(p)

out_main = OUT/'Brazilian_coffee_manuscript_refined.docx'
d.save(out_main)

s = Document(SUPP)
set_par(s.paragraphs[1], ed[0])
sanitize_title(s)
set_par(s.paragraphs[23], 'The explanatory models used log productivity, municipality effects, municipality-specific trends and year effects. A sensitivity included municipality-specific odd–even terms. Climate predictors entered prespecified calendar windows. Multiway clustered intervals are exploratory with only 13 reporting years and multiple candidate exposures; conditional estimates are not causal damage effects.')
set_par(s.paragraphs[25], 'Held-out production years were 2020–2024, with models fitted to earlier production outcomes and no unavailable test-year fixed effects. Scores used identical eligible test records across model families within a species. Weather and sample eligibility were assembled retrospectively; this evaluates temporal generalization of associations rather than skill at an operational lead time.')
set_par(s.paragraphs[83], 'The shock diagnostics align production and adverse weather over several years. They do not isolate the effects of individual events from management, bienniality, prices, cultivar turnover or other contemporaneous factors.')
set_par(s.paragraphs[90], 'Productivity coefficients are conditional associations and are susceptible to omitted management and correlated weather. Multiple windows and a short annual series make the intervals exploratory; they are not causal climate-damage estimates.')
set_par(s.paragraphs[91], 'Chronological validation tests temporal generalization within a retrospectively selected sample. It does not establish an operational forecast lead time or an information set available before harvest.')
for p in list(s.paragraphs):
    if 'Supplementary Figure S6.' in p.text and p.style.name=='Figure Caption':
        set_par(p, 'Supplementary Figure S6. Extended seasonal water-satisfaction, productivity-association and temporal-validation diagnostics. Water-balance scenario ranges represent variation over 12 CAD × Kc combinations and are not confidence intervals. Model coefficient intervals are exploratory; chronological predictions use retrospectively assembled weather histories.')
        break
# Keep supplementary visual components intact; replace title and remove superfluous blank references paragraphs.
for p in list(s.paragraphs):
    if p.text=='' and not p._element.xpath('.//w:drawing') and not p._element.xpath('.//w:br'):
        if p.style.name=='Normal' and p._element.getprevious() is not None and p._element.getnext() is not None:
            prev=p._element.getprevious(); nex=p._element.getnext()
            if prev.tag.endswith('}p') and nex.tag.endswith('}p'):
                delete_par(p)
s.save(OUT/'Brazilian_coffee_supplementary_refined.docx')
print(out_main)
