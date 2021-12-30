# import pandas as pd
#
# def create():
#
#
# # create all possible filters that may be applied to the results of each question
#
# filters = []
# filternames = []
# # extent of external questions.data usage
# filter_only = questions.Q1 == "Yes, as stand-alone training questions.data"
# filter_mix = questions.Q1 == "Yes, sometimes as enrichment and sometimes as stand-alone training questions.data"
# filter_enrich = questions.Q1 == "Yes, as enrichment of our internal questions.data"
# filters.append(filter_only)
# filters.append(filter_mix)
# filters.append(filter_enrich)
# filternames.append("Purely external questions.data for training")
# filternames.append("External questions.data purely or as enrichment")
# filternames.append("External questions.data as enrichment")
#
# # in-company use vs outside-of-company use
# filter_incompany = questions.Q2 == "Yes"
# filter_outcompany = questions.Q4 == "Yes"
# filters.append(filter_incompany)
# filters.append(filter_outcompany)
# filternames.append("Development inside of company")
# filternames.append("Development outside of company")
#
# # get the filters for large vs small companies
# print(type(questions.Q8))
# filter_small = questions.Q8.isin(["less than 10", "between 10 and 50", "between 50 and 250"])
# filter_large = questions.Q8.isin(["between 250 and 500", "between 500 and 1000", "more than 1000"])
# filters.append(filter_small)
# filters.append(filter_large)
# filternames.append("Companies with up to 250 employees")
# filternames.append("Companies with more than 250 employees")
#
# # get the filters for the different industries
# filter_art = questions.data["Art"] == 1
# filter_cons = questions.data["Construction"] == 1
# filter_corp = questions.data["Corporate"] == 1
# filter_edu = questions.data["Education"] == 1
# filter_fin = questions.data["Finance"] == 1
# filter_good = questions.data["Goods"] == 1
# filter_gov = questions.data['Government'] == 1
# filter_hlth = questions.data["Healthcare"] == 1
# filter_leg = questions.data["Legal"] == 1
# filter_man = questions.data["Manufacturing"] == 1
# filter_med = questions.data["Media & entertainment"] == 1
# filter_org = questions.data["Organization"] == 1
# filter_rec = questions.data["Recreation"] == 1
# filter_serv = questions.data["Service"] == 1
# filter_tech = questions.data["Technology"] == 1
# filter_tran = questions.data["Transportation"] == 1
# filters.append(filter_art)
# filters.append(filter_cons)
# filters.append(filter_corp)
# filters.append(filter_edu)
# filters.append(filter_fin)
# filters.append(filter_good)
# filters.append(filter_gov)
# filters.append(filter_hlth)
# filters.append(filter_leg)
# filters.append(filter_man)
# filters.append(filter_med)
# filters.append(filter_org)
# filters.append(filter_rec)
# filters.append(filter_serv)
# filters.append(filter_tech)
# filters.append(filter_tran)
# filternames.append("Art")
# filternames.append("Construction")
# filternames.append("Corporate")
# filternames.append("Education")
# filternames.append("Finance")
# filternames.append("Goods")
# filternames.append('Government')
# filternames.append("Healthcare")
# filternames.append("Legal")
# filternames.append("Manufacturing")
# filternames.append("Media & entertainment")
# filternames.append("Organization")
# filternames.append("Recreation")
# filternames.append("Service (industry group)")
# filternames.append("Technology")
# filternames.append("Transportation")
#
# # get the filters for the different application areas
# filter_inboundlogistics = questions.data["Inbound logistics"] == 1
# filter_production = questions.data["Production"] == 1
# filter_outboundlogistics = questions.data["Outbound logistics"] == 1
# filter_marketing = questions.data["Marketing, sales & distribution"] == 1
# filter_service = questions.data["Service.1"] == 1
# filter_company_infrastructure = questions.data["Company infrastructure"] == 1
# filter_HR_management = questions.data["HR management"] == 1
# filter_research_development = questions.data["R&D"] == 1
# filter_procurement = questions.data["Procurement"] == 1
# filter_finances_controlling = questions.data["Finances & controlling"] == 1
# filters.append(filter_inboundlogistics)
# filters.append(filter_production)
# filters.append(filter_outboundlogistics)
# filters.append(filter_marketing)
# filters.append(filter_service)
# filters.append(filter_company_infrastructure)
# filters.append(filter_HR_management)
# filters.append(filter_research_development)
# filters.append(filter_procurement)
# filters.append(filter_finances_controlling)
# filternames.append("Inbound Logistics")
# filternames.append("Production")
# filternames.append("Outbound logistics")
# filternames.append("Marketing, sales & distribution")
# filternames.append("Service (value chain)")
# filternames.append("Company infrastructure")
# filternames.append("HR management")
# filternames.append("Research & Development")
# filternames.append("Procurement")
# filternames.append("Finances & controlling")
#
# # filter for primary vs supporting activities
# primary_activities = questions.data[
#     ['Inbound logistics', 'Production', 'Outbound logistics', 'Marketing, sales & distribution', 'Service']]
# filter_primary = primary_activities.sum(1) > 0
# supporting_activities = questions.data[
#     ['Company infrastructure', 'HR management', 'R&D', 'Procurement', 'Finances & controlling']]
# filter_supporting = supporting_activities.sum(1) > 0
# filters.append(filter_primary)
# filters.append(filter_supporting)
# filternames.append("Primary activities of the value chain")
# filternames.append("Supporting activities of the value chain")
#
# # filter for traditional ML vs DL
# filter_ML = questions.Q26 < 45
# filter_DL = questions.Q26 > 55
# filters.append(filter_ML)
# filters.append(filter_DL)
# filternames.append("Traditional machine learning")
# filternames.append("Deep learning")
#
# # filter for structured vs unstructured questions.data
# filter_structured = questions.Q17 < 21
# filter_unstructured = questions.Q17 > 79
# filters.append(filter_structured)
# filters.append(filter_unstructured)
# filternames.append("Structured questions.data")
# filternames.append("Unstructured questions.data")
#
# # filter for automated framework usage
# filter_automated = questions.data["Automated frameworks (e.g. AutoSklearn, Autokeras)"] == 1
# filters.append(filter_automated)
# filternames.append("Using AutoML")
#
# # filter for those who named questions.data pools in last question
# datapool = questions.Q29.dropna().index.tolist()
# filter_datapool = questions.data.index.isin(datapool)
# filters.append(filter_datapool)
# filternames.append("Answered last question")
#
# sum_counts = []
# for filter in filters:
#     sum_counts.append(filter.sum())
#
# filter_df = pd.DataFrame(list(zip(filternames, sum_counts)),
#                          columns=['Filter name', 'Participants covered by filter'])
#
# filter_df = filter_df.sort_values(by='Participants covered by filter', ascending=False).reset_index()
#
# print("For the analysis of the results, a total of", len(filter_df.index),
#       "filters has been created to differentiate findings by company")
# print(
#     "size, application area regarding value chain (one by one and primary versus supporting activities, industry group")
# print("as well as whether the participants worked with ML in a company or developed ML models for organizational use")
# print("outside of one, such as e.g. researchers or government officials.")
# filter_df.drop(columns="index")
