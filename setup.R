pacman::p_load("dplyr", "ggplot2", "plotly", "reshape2", "forcats", "tidyr", "scales")
data <- read.csv("survey.csv", row.names = 1, stringsAsFactors = F)
to_address <- c("campus.feel.welcomed", "experienced.bullying.on.campus", 
                "num.courses.teach.per.semester", "num.courses.wish.teach",
                "teach.lower.divsion", "teach.upper.division", "teach.graduate.level",
                "teach.other.level","salary") # the other income columns
smart_name <- function(x) {
  gsub("(^|[^[:alnum:]])([[:alnum:]])", " \\U\\2", x, perl = TRUE) %>%
  trimws("left")
}
pal <- c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7")
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
### pie charts ###########
data %>% select(Religion) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = Freq/sum(Freq)) -> df_religion
data %>% select(Race) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = Freq/sum(Freq)) -> df_race
data %>% select(Gender) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = Freq/sum(Freq)) -> df_gender
data %>% select(Age) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = Freq/sum(Freq)) -> df_age
data %>% select(Category) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = Freq/sum(Freq)) -> df_faculty
data %>% select(starts_with("Sex")) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = Freq/sum(Freq)) -> df_sex
data %>% select(starts_with("College")) %>% table(exclude = NULL) %>% 
  data.frame() %>% mutate(Percent = Freq/sum(Freq)) -> df_college
height = 450
py_religion <- plot_ly(df_religion, labels = ~., values = ~Percent, type = 'pie', height = height,
              marker = list(colors = pal,line = list(color = '#FFFFFF', width = 1)),
              hoverinfo="label+percent") %>%
  layout(title = "Religious Diversity",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
py_race <- plot_ly(df_race, labels = ~., values = ~Percent, type = 'pie', height = height,
                   marker = list(colors = pal,line = list(color = '#FFFFFF', width = 1)),
                   hoverinfo="label+percent") %>%
  layout(title = 'Racial Diversity of Faculty At MSU Denver',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
py_age <- plot_ly(df_age, labels = ~., values = ~Percent, type = 'pie', height = height,
                  marker = list(colors = pal,line = list(color = '#FFFFFF', width = 1)),
                  hoverinfo="label+percent") %>%
  layout(title = 'Age Diversity of Faculty At MSU Denver',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
py_sex <- plot_ly(df_sex, labels = ~., values = ~Percent, type = 'pie', height = height,
                  marker = list(colors = pal,line = list(color = '#FFFFFF', width = 1)),
                  hoverinfo="label+percent") %>%
  layout(title = 'LGBTQ Diversity of Faculty At MSU Denver',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
py_gender <- plot_ly(df_gender, labels = ~., values = ~Percent, type = 'pie', height = height,
                     marker = list(colors = pal,line = list(color = '#FFFFFF', width = 1)),
                     hoverinfo="label+percent") %>%
  layout(title = 'Gender Diversity of Faculty At MSU Denver',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
py_college <- plot_ly(df_college, labels = ~., values = ~Percent, type = 'pie', height = height,
                      marker = list(colors = pal,line = list(color = '#FFFFFF', width = 1)),
                      hoverinfo="label+percent") %>%
  layout(title = 'College Distribution of Faculty At MSU Denver',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
py_faculty <- plot_ly(df_faculty, labels = ~., values = ~Percent, type = 'pie', height = height,
                      marker = list(colors = pal,line = list(color = '#FFFFFF', width = 1)),
                      hoverinfo="label+percent") %>%
  layout(title = 'Proportion of Affiliate Faculty At MSU Denver',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
### bar chats #########
bar_religion <- ggplot(df_religion,aes(x=.,y=Percent)) + geom_col(fill=pal[1]) + 
  scale_y_continuous(labels = percent) + theme_bw(base_size=16) + 
  theme(axis.text.x = element_text(angle = 10, hjust = 1, vjust = 0.5)) +
  ggtitle("Religion")
bar_race <- ggplot(df_race,aes(x=.,y=Percent)) + geom_col(fill=pal[2]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Race")
bar_sex <- ggplot(df_sex,aes(x=.,y=Percent)) + geom_col(fill=pal[3]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Sexual Orientation")
bar_gender <- ggplot(df_gender,aes(x=.,y=Percent)) + geom_col(fill=pal[4]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Gender")
bar_age <- ggplot(df_age,aes(x=.,y=Percent)) + geom_col(fill=pal[5]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Age")
bar_college <- ggplot(df_college,aes(x=.,y=Percent)) + geom_col(fill=pal[6]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("College of")
bar_faculty <- ggplot(df_faculty,aes(x=.,y=Percent)) + geom_col(fill=pal[7]) + 
  scale_y_continuous(labels = percent) + theme_bw() + ggtitle("Faculty Appointment")
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
#
### facted above graphs #####
witness$Type = rep("Witnessed", nrow(witness)) 
discrim$Type = rep("Experienced", nrow(discrim))  
allDiscrim <- rbind(witness,discrim) 

# ggplot(allDiscrim) + geom_col(aes(x=category,y=count, fill=gender)) +
#   coord_flip() + facet_wrap(facets = ~type) -> p
ggplot(allDiscrim,aes(text=plot_text)) + 
  geom_col(aes(x=Category,y=Percent, fill=Gender),position = "dodge") +
  scale_y_continuous(labels = percent, name = "Percent") +
  coord_flip() + facet_wrap(facets = ~Type) + xlab("") +
  theme(panel.spacing = unit(1, "lines")) + 
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
  scale_y_continuous(labels = percent, name = "Percent") + xlab("") +
  facet_wrap(facets = ~Category) + theme_bw(base_size = 16) -> p1
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
ggplot(comfort, aes(text = plot_text)) + 
  geom_col(aes(x=Category,y=Percent, fill=Type),position = "dodge") +
  scale_y_continuous(labels = percent, name = "Percent") + xlab("") +
  coord_flip() + theme_bw(base_size = 16) -> p2
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
  mutate(Category = factor(Category,levels = Category)) %>%
  ggplot(aes(text=plot_text)) + geom_col(aes(Category, Percent,fill=Type)) + coord_flip() + 
  facet_wrap(facets = ~Type) + theme_bw(base_size = 16) + 
  scale_y_continuous(labels=percent) -> value_plot
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
  mutate(plot_text = paste0(round(Percent*100,2),"%")) %>%
  ggplot(aes(text=plot_text)) + geom_col(aes(Category, Percent, fill=Type),position = "dodge") + 
  coord_flip() + facet_wrap(facets = ~College) + theme_bw(base_size=16) + 
  scale_y_continuous(labels=percent)-> pay_plot
#
### Group Effective #####################
data[,c(1,22:28)] %>%
  rename(Type = Category) %>%
  gather(Category, Response, -one_of("Type")) %>%
  filter(!is.na(Response)) %>% 
  group_by(Category,Type) %>%
  summarise(Percent=sum(Response=="Effective")/n()) %>%
  group_by(Type) %>% arrange(Percent) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) %>%
  ggplot(aes(text=plot_text)) + 
  geom_col(aes(Category,Percent, fill=Type),position = "dodge") +
  coord_flip() + scale_y_continuous(labels=percent,"Percent") +
  ggtitle("Percent Response on Group Effectiveness") + theme_bw(base_size=16) -> group_plot
#
### affil plot #####
data %>% select(`Affil Health Care Benefits`:`Affil Travel Reimbursement`) %>% 
  gather(Category, Response) %>% group_by(Category) %>%
  summarise(Percent=mean(Response=="Important",na.rm=T)) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) %>%
  ggplot(aes(x=Category,y=Percent,text=plot_text)) + geom_col() + scale_y_continuous(labels=percent) + 
  theme_bw(base_size=16) + ggtitle("Important?") + coord_flip() -> affil_plot
### retention plot #####
data %>% select(`Actively Seeking Other Employment`, contains("Plan To"), Category) %>% 
  rename(Type=Category) %>% gather(Category, Response, -one_of("Type")) %>% 
  group_by(Type,Category) %>% summarise(Percent=mean(Response=="Agree",na.rm = T)) %>%
  arrange(Percent) %>% mutate(Category=factor(Category, levels = Category)) %>%
  mutate(plot_text = paste0(round(Percent*100,2),"%")) %>%
  ggplot(aes(Category,Percent,text=plot_text)) + geom_col(aes(fill=Type),position = "dodge") + coord_flip() + theme_bw(base_size=16) +
  scale_y_continuous(labels=percent) + ggtitle("Retention") -> retention_plot