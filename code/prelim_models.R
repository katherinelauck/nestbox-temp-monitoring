##### Preliminary modeling

library(tidyverse)
require(ggeffects)
require(lme4)
require(viridis)

g <- read_rds("data/growth.rds") %>%
  filter(!is.na(gweight),!is.infinite(gweight))

# Does habitat affect growth interactively with temp?

meanI_WEBL <- lmerTest::lmer(gweight~poly(meanmaxtempI,2) * habitat + juliandate + (1|Nestbox),data = filter(g,Species == "WEBL",!is.nan(meanmaxtempI)))
meanI_WEBL_nosq <- lmerTest::lmer(gweight~meanmaxtempI * habitat + juliandate + (1|Nestbox),data = filter(g,Species == "WEBL",!is.nan(meanmaxtempI)))
anova(meanI_WEBL,meanI_WEBL_nosq)
meanI_WEBL_nosqint <- lmerTest::lmer(gweight~meanmaxtempI * habitat + poly(meanmaxtempI,2) + juliandate + (1|Nestbox),data = filter(g,Species == "WEBL",!is.nan(meanmaxtempI)))
anova(meanI_WEBL,meanI_WEBL_nosqint)
meanI_WEBL_noint <- lmerTest::lmer(gweight~poly(meanmaxtempI,2) + habitat + juliandate + (1|Nestbox),data = filter(g,Species == "WEBL",!is.nan(meanmaxtempI)))
anova(meanI_WEBL,meanI_WEBL_noint)
(p <- ggpredict(meanI_WEBL,terms = c("meanmaxtempI [all]","habitat")) %>% plot(line.size = 1.5,
                                                                              alpha = .2) +
    theme_classic() +
    xlab("Maximum temperature (z-scaled)") +
    ylab("Nestling growth (z-scaled)") +
    labs(title = element_blank(),color = "Cover type") +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    theme(text = element_text(size = 16))
)
ggsave("figures/growthbymeanI_WEBL.png",p,width = 6,height = 4)


(p <- ggpredict(meanI_WEBL,terms = c("meanmaxtempI [all]","habitat")) %>% plot(line.size = 1.5,
                                                                    alpha = .2) +
    theme_classic() +
    xlab("Maximum temperature (z-scaled)") +
    ylab("Nestling growth (z-scaled)") +
    labs(title = element_blank(),color = "Cover type") +
    scale_fill_manual(values = c("transparent","transparent","transparent","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    scale_color_manual(values = c("transparent","transparent","transparent","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    theme(text = element_text(size = 16),legend.title = element_text(color = "transparent"),
          legend.text = element_text(color = "transparent"),axis.text = element_text(color = "transparent"),axis.ticks = element_line(color = "transparent"),axis.line = element_line(color = "transparent"),axis.title = element_text(color = "transparent"),panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent"),panel.border = element_rect(color = "transparent",fill = "transparent"),legend.background = element_rect(fill = "transparent"),legend.box.background = element_rect(color = "transparent",fill = "transparent"))
)
ggsave("figures/growthbymeanI_WEBL_rowcrop.png",p,bg = "transparent",width = 6,height = 4)
(p <- ggpredict(meanI_WEBL,terms = c("meanmaxtempI [all]","habitat")) %>% plot(line.size = 1.5,
                                                                    alpha = .2) +
    theme_classic() +
    xlab("Maximum temperature (z-scaled)") +
    ylab("Nestling growth (z-scaled)") +
    labs(title = element_blank(),color = "Cover type") +
    scale_fill_manual(values = c("#440154FF","transparent","transparent","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    scale_color_manual(values = c("#440154FF","transparent","transparent","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    theme(text = element_text(size = 16),legend.title = element_text(color = "transparent"),
          legend.text = element_text(color = "transparent"),axis.text = element_text(color = "transparent"),axis.ticks = element_line(color = "transparent"),axis.line = element_line(color = "transparent"),axis.title = element_text(color = "transparent"),panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent"),panel.border = element_rect(color = "transparent",fill = "transparent"),legend.background = element_rect(fill = "transparent"),legend.box.background = element_rect(color = "transparent",fill = "transparent"))
)
ggsave("figures/growthbymeanI_WEBL_rowcropforest.png",p,bg = "transparent",width = 6,height = 4)
(p <- ggpredict(meanI_WEBL,terms = c("meanmaxtempI [all]","habitat")) %>% plot(line.size = 1.5,
                                                                    alpha = .2) +
    theme_classic() +
    xlab("Weekly mean maximum temperature") +
    ylab("Weekly nestling growth") +
    labs(title = element_blank(),color = "Cover type") +
    scale_fill_manual(values = c("#440154FF","transparent","#35B779FF","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    scale_color_manual(values = c("#440154FF","transparent","#35B779FF","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    theme(text = element_text(size = 16),legend.title = element_text(color = "transparent"),
          legend.text = element_text(color = "transparent"),axis.text = element_text(color = "transparent"),axis.ticks = element_line(color = "transparent"),axis.line = element_line(color = "transparent"),axis.title = element_text(color = "transparent"),panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent"),panel.border = element_rect(color = "transparent",fill = "transparent"),legend.background = element_rect(fill = "transparent"),legend.box.background = element_rect(color = "transparent",fill = "transparent"))
)
ggsave("figures/growthbymeanI_WEBL_rowcropforestgrassland.png",p,bg = "transparent",width = 6,height = 4)

# canopy?

meanI_WEBL_canopy <- lmerTest::lmer(gweight~poly(meanmaxtempI,2) * canopy_cover + juliandate + (1|Nestbox),data = filter(g,Species == "WEBL",!is.nan(meanmaxtempI)))
meanI_WEBL_canopy_noint <- lmerTest::lmer(gweight~poly(meanmaxtempI,2) + canopy_cover + juliandate + (1|Nestbox),data = filter(g,Species == "WEBL",!is.nan(meanmaxtempI)))
anova(meanI_WEBL_canopy,meanI_WEBL_canopy_noint)
(p <- ggpredict(meanI_WEBL_canopy,terms = c("meanmaxtempI","canopy_cover")) %>% plot(line.size = 1.5,
                                                                         alpha = .2) +
    theme_classic() +
    xlab("Maximum temperature (z-scaled)") +
    ylab("Nestling growth (z-scaled)") +
    labs(title = element_blank(),color = "canopy_cover") +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    theme(text = element_text(size = 16))
)

ggsave("figures/growthbymeanI_WEBL_canopy.png",p,width = 6,height = 4)

# back to original q with tres

meanI_TRES <- lmerTest::lmer(gweight~poly(meanmaxtempI,2) * habitat + juliandate + (1|Nestbox),data = filter(g,Species == "TRES",!is.nan(meanmaxtempI)))
meanI_TRES_nosqint <- lmerTest::lmer(gweight~meanmaxtempI * habitat + poly(meanmaxtempI,2) + juliandate + (1|Nestbox),data = filter(g,Species == "TRES",!is.nan(meanmaxtempI)))
meanI_TRES_nosq <- lmerTest::lmer(gweight~meanmaxtempI * habitat + juliandate + (1|Nestbox),data = filter(g,Species == "TRES",!is.nan(meanmaxtempI)))
anova(meanI_TRES,meanI_TRES_nosq)
meanI_TRES_noint <- lmerTest::lmer(gweight~poly(meanmaxtempI,2) + habitat + juliandate + (1|Nestbox),data = filter(g,Species == "TRES",!is.nan(meanmaxtempI)))
anova(meanI_TRES,meanI_TRES_noint)
(p <- ggpredict(meanI_TRES_nosq,terms = c("meanmaxtempI [all]","habitat")) %>% plot(line.size = 1.5,
                                                                         alpha = .2) +
    theme_classic() +
    xlab("Maximum temperature (z-scaled)") +
    ylab("Nestling growth (z-scaled)") +
    labs(title = element_blank(),color = "Cover type") +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    theme(text = element_text(size = 16))
)
ggsave("figures/growthbymeanI_TRES.png",p,width = 6,height = 4)


(p <- ggpredict(meanI_TRES_nosq,terms = c("meanmaxtempI","habitat")) %>% plot(line.size = 1.5,
                                                                         alpha = .2) +
    theme_classic() +
    xlab("Maximum temperature (z-scaled)") +
    ylab("Nestling growth (z-scaled)") +
    labs(title = element_blank(),color = "Cover type") +
    scale_fill_manual(values = c("transparent","transparent","transparent","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    scale_color_manual(values = c("transparent","transparent","transparent","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    theme(text = element_text(size = 16),legend.title = element_text(color = "transparent"),
          legend.text = element_text(color = "transparent"),axis.text = element_text(color = "transparent"),axis.ticks = element_line(color = "transparent"),axis.line = element_line(color = "transparent"),axis.title = element_text(color = "transparent"),panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent"),panel.border = element_rect(color = "transparent",fill = "transparent"),legend.background = element_rect(fill = "transparent"),legend.box.background = element_rect(color = "transparent",fill = "transparent"))
)
ggsave("figures/growthbymeanI_TRES_rowcrop.png",p,bg = "transparent",width = 6,height = 4)
(p <- ggpredict(meanI_TRES_nosq,terms = c("meanmaxtempI","habitat")) %>% plot(line.size = 1.5,
                                                                         alpha = .2) +
    theme_classic() +
    xlab("Maximum temperature (z-scaled)") +
    ylab("Nestling growth (z-scaled)") +
    labs(title = element_blank(),color = "Cover type") +
    scale_fill_manual(values = c("#440154FF","transparent","transparent","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    scale_color_manual(values = c("#440154FF","transparent","transparent","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    theme(text = element_text(size = 16),legend.title = element_text(color = "transparent"),
          legend.text = element_text(color = "transparent"),axis.text = element_text(color = "transparent"),axis.ticks = element_line(color = "transparent"),axis.line = element_line(color = "transparent"),axis.title = element_text(color = "transparent"),panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent"),panel.border = element_rect(color = "transparent",fill = "transparent"),legend.background = element_rect(fill = "transparent"),legend.box.background = element_rect(color = "transparent",fill = "transparent"))
)
ggsave("figures/growthbymeanI_TRES_rowcropforest.png",p,bg = "transparent",width = 6,height = 4)
(p <- ggpredict(meanI_TRES_nosq,terms = c("meanmaxtempI","habitat")) %>% plot(line.size = 1.5,
                                                                         alpha = .2) +
    theme_classic() +
    xlab("Weekly mean maximum temperature") +
    ylab("Weekly nestling growth") +
    labs(title = element_blank(),color = "Cover type") +
    scale_fill_manual(values = c("#440154FF","transparent","#35B779FF","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    scale_color_manual(values = c("#440154FF","transparent","#35B779FF","#FDE725FF"),guide = guide_legend(override.aes = list(alpha = 0) )) +
    theme(text = element_text(size = 16),legend.title = element_text(color = "transparent"),
          legend.text = element_text(color = "transparent"),axis.text = element_text(color = "transparent"),axis.ticks = element_line(color = "transparent"),axis.line = element_line(color = "transparent"),axis.title = element_text(color = "transparent"),panel.background = element_rect(fill = "transparent"),plot.background = element_rect(fill = "transparent"),panel.border = element_rect(color = "transparent",fill = "transparent"),legend.background = element_rect(fill = "transparent"),legend.box.background = element_rect(color = "transparent",fill = "transparent"))
)
ggsave("figures/growthbymeanI_TRES_rowcropforestgrassland.png",p,bg = "transparent",width = 6,height = 4)

# canopy?
meanI_TRES_canopy <- lmerTest::lmer(gweight~poly(meanmaxtempI,2) * canopy_cover + juliandate + (1|Nestbox),data = filter(g,Species == "TRES",!is.nan(meanmaxtempI)))
meanI_TRES_canopy_nosq <- lmerTest::lmer(gweight~meanmaxtempI * canopy_cover + juliandate + (1|Nestbox),data = filter(g,Species == "TRES",!is.nan(meanmaxtempI)))
anova(meanI_TRES_canopy,meanI_TRES_canopy_nosq)
meanI_TRES_canopy_nosqint <- lmerTest::lmer(gweight~meanmaxtempI * canopy_cover + poly(meanmaxtempI,2) + juliandate + (1|Nestbox),data = filter(g,Species == "TRES",!is.nan(meanmaxtempI)))
anova(meanI_TRES_canopy,meanI_TRES_canopy_nosqint)
meanI_TRES_canopy_noint <- lmerTest::lmer(gweight~poly(meanmaxtempI,2) + canopy_cover + juliandate + (1|Nestbox),data = filter(g,Species == "TRES",!is.nan(meanmaxtempI)))
anova(meanI_TRES_canopy,meanI_TRES_canopy_noint)
(p <- ggpredict(meanI_TRES_canopy,terms = c("meanmaxtempI","canopy_cover")) %>% plot(line.size = 1.5,
                                                                                     alpha = .2) +
    theme_classic() +
    xlab("Maximum temperature (z-scaled)") +
    ylab("Nestling growth (z-scaled)") +
    labs(title = element_blank(),color = "Canopy cover") +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    theme(text = element_text(size = 16))
)
ggsave("figures/growthbymeanI_TRES_canopy_sq.png",p,width = 6,height = 4)
