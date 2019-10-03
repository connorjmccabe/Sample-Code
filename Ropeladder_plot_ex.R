require(RCurl)
require(ggplot2)
require(RColorBrewer)
require(dplyr)

df<-read.csv(text=getURL("https://raw.githubusercontent.com/connorjmccabe/Sample-Code/master/sampleparams.csv"))
head(df)

#Sample code of how to do some clean-up:
rummj_smw.params.clean<-rummj_smw.params %>%
  mutate(param=as.character(param)) %>%
  # This recodes the ordinal axis to something more sensible compared to lavaan output. Just 2 examples:
  mutate(param = ifelse(param == "rum_int~smw", "SM Status->Rumination Int.", param),
         param = ifelse(param == "rum_slope~smw", "SM Status->Rumination Slope", param))%>% #etc.
  #This removes anything from the model that I do not want to include in the plot
  dplyr::filter(param !="rum_int=~RUMXSc_17" &
                  param !="rum_int=~RUMXSc_18" &
                  param !="rum_int=~RUMXSc_19" &
                  param !="rum_int=~RUMXSc_20" &
                  param !="rum_slope=~RUMXSc_18" &
                  param !="rum_slope=~RUMXSc_19" &
                  param !="rum_slope=~RUMXSc_20" &
                  param !="mj_int=~nadu_mj_18" &
                  param !="mj_int=~nadu_mj_19" &
                  param !="mj_int=~nadu_mj_20" &
                  param !="mj_slope=~nadu_mj_18" &
                  param !="mj_slope=~nadu_mj_19" &
                  param !="rum_int~white" &
                  param !="rum_int~other" &
                  param !="rum_slope~white" &
                  param !="rum_slope~other" &
                  param !="mj_int~white" &
                  param !="mj_int~other" &
                  param !="smw~white" &
                  param !="smw~other" &
                  param !="mj_slope~white" &
                  param !="mj_slope~other" &
                  param !="white~1" &
                  param !="other~1" &
                  param !="white~~other" &
                  param !="rum_int~1" &
                  param !="mj_int~1" &
                  param !="smw~1" &
                  param !="smw~~smw")

(rummjparamplot<-ggplot(rummj_smw.params.clean, aes(x = param, y = estimate, color=model),size=.1) + 
    geom_pointrange(aes(ymax = upper_bound, ymin = lower_bound, color=model), size=.2, position = position_dodge(width=.5)) +
    # geom_text(aes(label = estimate), nudge_x = 0.15) + 
    scale_x_discrete("") + 
    scale_colour_brewer(type = "qual", palette = 2, direction = 1) +
    geom_hline(yintercept = 0, color = "red") + 
    theme_bw() + 
    theme(text = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ylab(NULL) + 
    ggtitle("Regressing Growth Processes on SM Status: Marijuana Use") +
    coord_flip())
# facet_grid(~model)

ggsave("rummjparamplot.pdf",rummjparamplot,width=8, height=7,units="in")