pacman::p_load("dplyr", "ggplot2", "plotly", "reshape2", "forcats", "tidyr", "scales")
data <- read.csv("survey.csv", row.names = 1, stringsAsFactors = F)
#
### to address ##############################
to_address <- c("campus.feel.welcomed", "experienced.bullying.on.campus", 
                "num.courses.teach.per.semester", "num.courses.wish.teach",
                "teach.lower.divsion", "teach.upper.division", "teach.graduate.level",
                "teach.other.level","salary") # the other income columns
### filters ###############
characteristics <- c("type", "gender", 
                     "college.school.of.department", 
                     "tenure", "rank", "religion","sexual.orientation",
                     "religion", "age", "disability","veteran")
data %>% select(matches("num|rank")) %>% names() -> morech
characteristics <- c(characteristics,morech)
### pie charts ###########
data %>% select(religion) %>% table() %>% 
  data.frame() %>% mutate(perc = Freq/sum(Freq)) -> df
py <- plot_ly(df, labels = ~., values = ~perc, type = 'pie') %>%
  layout(title = 'Religous Diversity of Faculty At MSU Denver',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
### Discrim plot #########
data %>% select(starts_with("discriminated"),gender) -> discrim
colnames(discrim) <- sub("discriminated.","", colnames(discrim))
discrim %>%
  gather(category, response,starts_with("for")) %>%
  filter(!is.na(response)) %>%
  group_by(category, gender) %>% 
  summarise(count = sum(response=="Yes"),
            perc = count/n()) %>%
  group_by(gender) %>% mutate(category=smart_name(category)) %>%
  arrange(count) %>% 
  mutate(category=factor(category, levels = category)) -> discrim
  
  ggplot(discrim) + geom_col(aes(x=category,y=count, fill=gender)) +
  coord_flip() -> g
#ggplotly(g)
### witness discrimination ######
levs <- levels(discrim$category)
data %>% select(starts_with("witness"),gender) -> witness
colnames(witness) <- sub("witnessed.discrimination.","", colnames(witness))
witness %>%
  gather(category, response,starts_with("for")) %>%
  filter(!is.na(response)) %>%
  group_by(category, gender) %>% 
  summarise(count = sum(response=="Yes"),
            perc = count/n()) %>%
  group_by(gender) %>% mutate(category=factor(category, levels = levs)) -> witness
  ggplot(witness) + geom_col(aes(x=category,y=count, fill=gender)) +
  coord_flip() -> g1
#ggplotly(g1)
### facted above graphs #####
witness$type = rep("Witnessed", nrow(witness)) 
discrim$type = rep("Experienced", nrow(discrim))  
allDiscrim <- rbind(witness,discrim) 

# ggplot(allDiscrim) + geom_col(aes(x=category,y=count, fill=gender)) +
#   coord_flip() + facet_wrap(facets = ~type) -> p
ggplot(allDiscrim) + 
  geom_col(aes(x=category,y=perc, fill=gender),position = "dodge") +
  scale_y_continuous(labels = percent, name = "Percent") +
  coord_flip() + facet_wrap(facets = ~type) + 
  theme_bw(base_size = 16) -> p
ggplotly(p) %>% config(collaborate = F, displaylogo=F)
### likelihod to report discrim ########
data %>% rename(college=college.school.of.department) %>%
  select(contains("report"), gender, category, college) %>% 
  gather(type, response, contains("report")) %>%
  filter(!is.na(response)) %>%
  group_by(type, gender, category) %>% 
  summarise(count = sum(response=="Likely"),
            perc = count/n()) %>%
  group_by(gender, category) %>% arrange(count) %>% 
  mutate(type=factor(type, levels = type)) -> report
ggplot(report) + geom_col(aes(type, perc, fill=gender),position = "dodge") + 
  scale_y_continuous(labels = percent, name = "Percent") +
  facet_wrap(facets = ~category) + theme_bw(base_size = 16) -> p1
#ggplotly(p1)


### comfort disclosing #####
data %>% select(starts_with("comfort"),gender) -> comfort
colnames(comfort) <- sub("comfort.","", colnames(comfort))
comfort %>%
  gather(category, response,starts_with("disclos")) %>%
  filter(!is.na(response)) %>%
  group_by(category, gender) %>% 
  summarise(count = sum(response=="Comfortable"),
            perc = count/n()) %>%
  group_by(gender) %>% arrange(desc(count)) %>%
  mutate(category=factor(category, levels = category)) -> comfort
ggplot(comfort) + 
  geom_col(aes(x=category,y=perc, fill=gender),position = "dodge") +
  scale_y_continuous(labels = percent, name = "Percent") + 
  coord_flip() + theme_bw() -> p2
#ggplotly(p2)
  
### Age based discrimination #########
data %>% select(age, discriminated.for.age) %>% table() %>% 
  data.frame() %>% group_by(age) %>% 
  summarise(perc = sum(discriminated.for.age=="Yes")/sum(Freq)) %>%
  ggplot() + geom_col(aes(age,perc)) + 
  scale_y_continuous(labels=percent,"Percent") + 
  coord_flip() + theme_bw() + ggtitle("Experienced Discrimination")
data %>% select(age, discriminated.for.age) %>% 
  filter(discriminated.for.age=="Yes") %>% 
  ggplot() + geom_bar(aes(age,y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent,"Percent") + 
  theme_bw() + ggtitle("Distribution of Reported Age Discrimination")
### value ##############
data %>% select(contains("value"),
                contains("feed"), 
                contains("good"), category) %>%
  rename(type = category) %>%
  gather(category, response, -one_of("type")) %>%
  filter(!is.na(response)) %>%
  group_by(category,type) %>%
  summarise(count = sum(response=="Agree"),
            perc = count/n()) %>% group_by(type) %>%
  arrange(desc(perc)) %>% 
  mutate(category = factor(category,levels = category)) %>%
  ggplot() + geom_col(aes(category, perc)) + coord_flip() + facet_wrap(facets = ~type)
#
### compensation #######
data %>% select(contains("Compensated"), Category, starts_with("College")) %>%
  rename("College"="College School Of Department", Type = Category) %>%
  gather(Category, Response, contains("Compensate")) %>%
  filter(!is.na(Response)) %>% 
  group_by(Category,Type, College) %>%
  summarise(count = sum(Response=="Agree"),
            Percent = count/n()) %>% group_by(Type) %>%
  arrange(desc(Percent)) %>% 
  group_by(Type, College) %>%
  mutate(Category = factor(Category,levels = Category)) %>%
  ggplot() + geom_col(aes(Category, Percent, fill=Type),position = "dodge") + 
  coord_flip() + facet_wrap(facets = ~College) -> pay_plot
### expectations, respectful students, support ####
data %>% select(`Students Respectful`,contains("support"),Category) %>%
  rename(Type = Category) %>%
  gather(Category, Response, -one_of("Type")) %>%
  filter(!is.na(Response)) %>% 
  group_by(Category,Type) %>%
  summarise(Percent=sum(Response=="Agree")/n()) %>%
  group_by(Type) %>% arrange(Percent) %>%
  ggplot() + 
  geom_col(aes(Category,Percent, fill=Type),position = "dodge") +
  coord_flip() -> df_st_respect
data %>% select(contains("Expect"), Category) %>%
  rename(Type = Category) %>% 
  gather(Category, Response, contains("Expect"))%>%
  filter(!is.na(Response)) %>%
  group_by(Category,Type) %>% 
  summarise(Percent=sum(Response=="Reasonable")/n()) %>%
  group_by(Type) %>% arrange(Percent) %>%
  ggplot() + 
  geom_col(aes(Category,Percent, fill=Type),position = "dodge")
### office, chair, senate, union, associ, chapter #######
data[,c(1,22:28)] %>%
  rename(Type = Category) %>%
  gather(Category, Response, -one_of("Type")) %>%
  filter(!is.na(Response)) %>% 
  group_by(Category,Type) %>%
  summarise(Percent=sum(Response=="Effective")/n()) %>%
  group_by(Type) %>% arrange(Percent) %>%
  ggplot() + 
  geom_col(aes(Category,Percent, fill=Type),position = "dodge") +
  coord_flip() + scale_y_continuous(labels=percent,"Percent") +
  ggtitle("Percent Response on Group Effectiveness")
### rtp, salary, rank, benefits, summer pay ########
data %>% select(matches("Rtp|Salary|Rank|Benefits|Summer")) %>% names()
### tution travel #####
### affil stufff #########
data %>% select(`Affil Health Care Benefits`:`Affil Travel Reimbursement`) %>% 
  gather(Category, Response) %>% group_by(Category) %>%
  summarise(Percent=mean(Response=="Important",na.rm=T)) %>%
  ggplot(aes(x=Category,y=Percent)) + geom_col() + scale_y_continuous(labels=percent) + 
  theme_bw() + ggtitle("Important?") + coord_flip() -> affil_plot
### affil fulltime retention ####
data %>% select(`Actively Seeking Other Employment`, contains("Plan To"), Category) %>% 
  rename(Type=Category) %>% gather(Category, Response, -one_of("Type")) %>% 
  group_by(Type,Category) %>% summarise(Percent=mean(Response=="Agree",na.rm = T)) %>%
  arrange(Percent) %>% mutate(Category=factor(Category, levels = Category)) %>%
  ggplot(aes(Category,Percent)) + geom_col(aes(fill=Type),position = "dodge") + coord_flip() + theme_bw() +
  scale_y_continuous(labels=percent) + ggtitle("Retention") -> retention_plot
