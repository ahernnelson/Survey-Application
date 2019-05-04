pacman::p_load("dplyr", "ggplot2", "plotly", "reshape2", "forcats", "tidyr", "scales", "ggsci")
data <- read.csv("survey.csv", row.names = 1, stringsAsFactors = F)
to_address <- c("campus.feel.welcomed", "experienced.bullying.on.campus", 
                "num.courses.teach.per.semester", "num.courses.wish.teach",
                "teach.lower.divsion", "teach.upper.division", "teach.graduate.level",
                "teach.other.level","salary") # the other income columns
smart_name <- function(x) {
  gsub("(^|[^[:alnum:]])([[:alnum:]])", " \\U\\2", x, perl = TRUE) %>%
  trimws("left")
}
pal <- c("#999999", "#E69F00", "#56B4E9", "#000000", "#009E73", "#F0E442", "#D55E00", "#0072B2", "#CC79A7")
ggplot <- function(...){
  ggplot2::ggplot(...) + scale_fill_manual(values=rev(pal), na.value="gray") + 
    ylab("") + xlab("") + theme(legend.key.size = unit(1.5, "cm"),
                                legend.text = element_text(size = 15))
}
myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

#
### Rename columns ############
data %>% names() %>% smart_name() -> niceNames
colnames(data) <- niceNames
data %>% mutate(Category = smart_name(Category)) -> data
#
### filters ###############
characteristics <- c("type", "gender", 
                     "college.school.of.department", 
                     "tenure", "rank", "religion","sexual.orientation",
                     "religion", "age", "disability","veteran") %>% smart_name()
data %>% select(matches("Num|Rank")) %>% names() -> morech
characteristics <- c(characteristics,morech)
#
### data frames for bars ###########
data %>% select(Religion) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%"))-> df_religion
data %>% select(Race) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%")) -> df_race
data %>% select(Gender) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%")) -> df_gender
data %>% select(Age) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%")) -> df_age
data %>% select(Category) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%")) -> df_faculty
data %>% select(starts_with("Sex")) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%")) -> df_sex
data %>% select(starts_with("College")) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%")) -> df_college
data %>% select(Veteran) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%")) -> df_veteran
data %>% select(Disability) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = round(Freq/sum(Freq), 4)) %>% 
  mutate(plot_text = paste0(Percent*100,"%")) -> df_disable
### bar chats #########
bar_religion <- ggplot(df_religion,aes(x=.,y=Percent)) + geom_col(fill=pal[1]) + 
  scale_y_continuous(labels = percent) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 10, hjust = 1, vjust = 0.5)) +
  ggtitle("Religion")  
bar_race <- ggplot(df_race,aes(x=.,y=Percent, text = plot_text)) + geom_col(fill=pal[2]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Race")  
bar_sex <- ggplot(df_sex,aes(x=.,y=Percent, text = plot_text)) + geom_col(fill=pal[3]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Sexual Orientation")
bar_gender <- ggplot(df_gender,aes(x=.,y=Percent, text = plot_text)) + geom_col(fill=pal[4]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Gender")  
bar_age <- ggplot(df_age,aes(x=.,y=Percent, text = plot_text)) + geom_col(fill=pal[5]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Age") 
bar_college <- ggplot(df_college,aes(x=.,y=Percent, text = plot_text)) + geom_col(fill=pal[6]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("College") 
bar_faculty <- ggplot(df_faculty,aes(x=.,y=Percent, text = plot_text)) + geom_col(fill=pal[7]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Faculty Appointment") 
bar_veteran <- ggplot(df_veteran,aes(x=.,y=Percent, text = plot_text)) + geom_col(fill=pal[8]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Veteran") 
bar_disable <- ggplot(df_disable,aes(x=.,y=Percent, text = plot_text)) + geom_col(fill=pal[9]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Disable") 
#
### Discrim #########
data %>% select(starts_with("Discriminated"),Gender) -> discrim
colnames(discrim) <- sub("Discriminated ","", colnames(discrim))
discrim %>%
  gather(Category, Response,starts_with("For")) %>%
  filter(!is.na(Response)) %>%
  group_by(Category, Gender) %>% 
  summarise(count = sum(Response=="Yes"),
            Percent = count/n()) %>%
  group_by(Gender) %>% arrange(count) %>% 
  mutate(Category=factor(Category, levels = Category)) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) -> discrim
discrim %>% select(Category, Gender, count) %>%
  spread(Gender, count) -> discrim_table
#
### Witness ####
levs <- levels(discrim$Category)
data %>% select(starts_with("Witness"),Gender) -> witness
colnames(witness) <- sub("Witnessed Discrimination ","", colnames(witness))
witness %>%
  gather(Category,Response,starts_with("For")) %>%
  filter(!is.na(Response)) %>%
  group_by(Category, Gender) %>% 
  summarise(count = sum(Response=="Yes"),
            Percent = count/n()) %>%
  group_by(Gender) %>%
  mutate(Category=factor(Category, levels = levs)) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) -> witness
witness %>% select(Category, Gender, count) %>%
  spread(Gender, count) -> witness_table
#
#
### facted above graphs #####
witness$Type = rep("Witnessed", nrow(witness)) 
discrim$Type = rep("Experienced", nrow(discrim))  
allDiscrim <- rbind(witness,discrim) 

# ggplot(allDiscrim) + geom_col(aes(x=category,y=count, fill=gender)) +
#   coord_flip() + facet_wrap(facets = ~type) -> p
ggplot(allDiscrim,aes(text=plot_text)) + 
  geom_col(aes(x=Category,y=Percent, fill=Gender),position = "dodge") +
  scale_y_continuous(labels = percent) +
  coord_flip() + guides(fill = guide_legend(reverse = TRUE)) + 
  facet_wrap(facets = ~Type)  +
  theme(panel.spacing = unit(1, "lines"), legend.title = element_blank()) + 
  theme_bw(base_size = 16) -> p
#
### likelihod to report discrim ########
data %>% rename('College'='College School Of Department') %>%
  select(contains("Report"), Gender, Category, College) %>% 
  gather(Type, Response, contains("Report")) %>%
  filter(!is.na(Response)) %>%
  group_by(Type, Gender, Category) %>% 
  summarise(count = sum(Response=="Likely"),
            Percent = count/n()) %>%
  group_by(Gender, Category) %>% arrange(count) %>% 
  mutate(Type=factor(Type, levels = Type)) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%"))-> report
ggplot(report,aes(text=plot_text)) + geom_col(aes(Type, Percent, fill=Gender),position = "dodge") + 
  scale_y_continuous(labels = percent)  +
  facet_wrap(facets = ~Category) + theme_bw(base_size = 16) -> p1
report %>% unite(temp,c(Gender,Category), sep = " ") %>% 
    select(count, Type, temp) %>% spread(temp,count) -> report_table
#
### comfort disclosing #####
data %>% select(starts_with("Comfort"), Category) -> comfort
colnames(comfort) <- sub("Comfort ","", colnames(comfort))
comfort %>% rename(Type = Category) %>%
  gather(Category, Response,starts_with("Disclos")) %>%
  filter(!is.na(Response)) %>%
  group_by(Category, Type) %>% 
  summarise(count = sum(Response=="Comfortable"),
            Percent = count/n()) %>%
  group_by(Type) %>% arrange(desc(count)) %>%
  mutate(Category=factor(Category, levels = Category)) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) -> comfort
comfort %>% select(Category, Type, count) %>% spread(Type, count) -> comfort_table
ggplot(comfort, aes(text = plot_text)) + 
  geom_col(aes(x=Category,y=Percent, fill=Type),position = "dodge") +
  scale_y_continuous(labels = percent)  +
  coord_flip() + guides(fill = guide_legend(reverse = TRUE)) + theme_bw(base_size = 16) -> p2
#
### Value ###########
data %>% select(contains("Value"),
                contains("Feed"), 
                contains("Good"), Category) %>%
  rename(Type = Category) %>%
  gather(Category, Response, -one_of("Type")) %>%
  filter(!is.na(Response)) %>%
  group_by(Category,Type) %>%
  summarise(count = sum(Response=="Agree"),
            Percent = count/n()) %>% 
  mutate(Type=smart_name(Type)) %>%
  group_by(Type) %>%
  arrange(desc(Percent)) %>% 
  mutate(plot_text = paste0(round(Percent*100,2),"%")) %>%
  mutate(Category = factor(Category,levels = Category)) -> value
  ggplot(value,aes(text=plot_text)) + geom_col(aes(Category, Percent,fill=Type)) + coord_flip() + guides(fill = guide_legend(reverse = TRUE))  + 
  facet_wrap(facets = ~Type) + theme_bw(base_size = 16) + 
  scale_y_continuous(labels=percent) -> value_plot
value %>% select(Category, Type, count) %>% spread(Type, count) -> value_table
#
### pay/compensated ####################
data %>% select(contains("Compensated"), Salary, Category, starts_with("College")) %>%
  rename("College"="College School Of Department", Type = Category, "Satisfied with Salary"="Salary") %>%
  gather(Category, Response, matches("Compensated|Salary")) %>%
  filter(!is.na(Response)) %>% 
  group_by(Category,Type, College) %>%
  summarise(count = sum(Response=="Agree" | Response == "Satisfied"),
            Percent = count/n()) %>% group_by(Type) %>%
  arrange(desc(Percent)) %>% 
  group_by(Type, College) %>%
  mutate(Category = factor(Category,levels = Category)) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) -> pay
  ggplot(pay,aes(text=plot_text)) + geom_col(aes(Category, Percent, fill=Type),position = "dodge") + 
  coord_flip() + guides(fill = guide_legend(reverse = TRUE)) + 
  facet_wrap(facets = ~College) + theme_bw(base_size=16) + 
  scale_y_continuous(labels=percent) -> pay_plot
pay %>% unite(temp,c(Type, College), sep=" ") %>%
  select(temp, Category, count) %>% spread(temp, count) -> pay_table
#
### Group Effective #####################
data[,c(1,22:28)] %>%
  rename(Type = Category) %>%
  gather(Category, Response, -one_of("Type")) %>%
  filter(!is.na(Response)) %>% 
  group_by(Category,Type) %>%
  summarise(count=sum(Response=="Effective"),
            Percent = count/n()) %>%
  group_by(Type) %>% arrange(Percent) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) -> group
  ggplot(group,aes(text=plot_text)) + 
  geom_col(aes(Category,Percent, fill=Type),position = "dodge") +
  coord_flip() + guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels=percent) +
  ggtitle("Percent Response on Group Effectiveness") + theme_bw(base_size=16)-> group_plot
group %>% select(Category, Type, count) %>% spread(Type,count) -> group_table
#
### affil plot #####
data %>% select(`Affil Health Care Benefits`:`Affil Travel Reimbursement`) %>% 
  gather(Category, Response) %>% 
  filter(!is.na(Response)) %>%
  group_by(Category) %>%
  summarise(Percent=mean(Response=="Important",na.rm=T),
            count = Percent * n()) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) -> affil
  ggplot(affil,aes(x=Category,y=Percent,text=plot_text)) + geom_col() + 
    scale_y_continuous(labels=percent) + theme_bw(base_size=16) + 
    ggtitle("Important?") + coord_flip() + 
    guides(fill = guide_legend(reverse = TRUE)) -> affil_plot
affil %>% select(Category, count) -> affil
### retention plot #####
data %>% select(`Actively Seeking Other Employment`, contains("Plan To"), Category) %>% 
  rename(Type=Category) %>% gather(Category, Response, -one_of("Type")) %>% 
  filter(!is.na(Response)) %>%
  group_by(Type,Category) %>% summarise(count=sum(Response=="Agree",na.rm = T),
                                        Percent = count / n()) %>%
  arrange(Percent) %>% mutate(Category=factor(Category, levels = Category)) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) -> retention
  ggplot(retention,aes(Category,Percent,text=plot_text)) + geom_col(aes(fill=Type),position = "dodge") + 
  coord_flip() + guides(fill = guide_legend(reverse = TRUE))  + theme_bw(base_size=16) +
  scale_y_continuous(labels=percent) + ggtitle("Retention")  -> retention_plot
retention %>% select(Type, Category, count) %>% spread(Type,count) -> retention_table
### List of all plots for final touches ##############
group_plot <- group_plot + theme(legend.title = element_blank())
p <- p + theme(legend.title = element_blank())
p1 <- p1 + theme(legend.title = element_blank())
p2 <- p2 + theme(legend.title = element_blank())
pay_plot <- pay_plot + theme(legend.title = element_blank())
retention_plot <- retention_plot + theme(legend.title = element_blank())
value_plot <- value_plot + theme(legend.title = element_blank())
### List of tables for final touches ##########################
group_table <- rename(group_table, "Response Group Effective" = "Category")
discrim_table <- rename(discrim_table, Experienced = Category)
witness_table <- rename(witness_table, Witnessed = Category)
report_table <- rename(report_table, "Would Report" = "Type")
comfort_table <- rename(comfort_table, "Is Comfortable" = "Category")
pay_table <- rename(pay_table, "Agree that" = "Category")
retention_table <- rename(retention_table, "Responded yes to" = "Category")
value_table <- rename(value_table, "Agree that" = "Category")
affil_table <- rename(affil, "Is Important" = "Category")
