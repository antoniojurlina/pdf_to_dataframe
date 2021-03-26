library(stringr)
library(pdftools)
library(rebus)
library(ggplot2)
library(here)

paste0(here(), "/data") %>% setwd()


text <- pdf_text("PUBLICINFO-April-2017-pages-deleted.pdf")
text2 <- strsplit(text, "\n")

text2 <- lapply(text2, function(x) {x[-c(1:4,43:44)]})
text2[[1]] <- text2[[1]][-1]
text2[[118]] <- text2[[118]][-(12:13)]

text3 <- unlist(text2)


public_info <- data.frame(1,2,3,4,5,6,7)
colnames(public_info) <- c("CMP", "Name", "Department", "Title", "Salary", "JobSt", "Bargaining_Unit")

for (i in 1:length(text3)) {
  CMP <- str_trim(str_extract(text3[[i]], pattern = START %R% ANY_CHAR %R% optional(one_or_more(WRD)) 
                            %R% optional(one_or_more(WRD)) %R% optional(one_or_more(WRD))))
  temp.name <- str_extract(text3[[i]], pattern = CMP %R% one_or_more(SPC) %R% one_or_more(WRD) %R%  optional(or(SPC, "-", "'")) %R% 
                           zero_or_more(WRD) %R% "," %R% SPC %R% one_or_more(WRD) %R% 
                           optional(SPC) %R% optional(char_class("A-Z")) %R% optional(DOT) %R% zero_or_more(WRD))
  temp.name <- str_extract(temp.name, pattern = one_or_more(SPC) %R% one_or_more(WRD) %R%  optional(or(SPC, "-", "'")) %R% 
                           zero_or_more(WRD) %R% "," %R% SPC %R% one_or_more(WRD) %R% 
                           optional(SPC) %R% optional(char_class("A-Z")) %R% optional(DOT) %R% zero_or_more(WRD))
  Name <- str_trim(temp.name)
  temp.vector <- str_extract(text3[[i]], pattern = SPC %R% char_class("A-Z") %R% char_class("A-Z") %R% 
              char_class("A-Z") %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z"))
            %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z"))
            %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z")))
  Department <- str_extract(temp.vector, pattern = char_class("A-Z") %R% char_class("A-Z") %R% 
              char_class("A-Z") %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z"))
            %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z"))
            %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z")) %R% optional(char_class("A-Z")))
  temp.vector <- str_extract(text3[[i]], pattern = Department %R% one_or_more(SPC) %R% one_or_more(WRD) %R% optional(SPC) %R% 
              optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
              %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)))
  temp.title <- str_extract(temp.vector, pattern = SPC %R% optional(SPC) %R% zero_or_more(SPC) %R% 
              one_or_more(WRD) %R% optional(SPC) %R% 
              optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD))
            %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)))
  Title <- str_trim(temp.title)
  temp.salary <- str_extract(text3[[i]], pattern = DGT %R% optional(DGT) %R% optional(DGT) %R% optional(DGT) 
                           %R% optional(DGT) %R% optional(DGT) %R% DOT %R% DGT %R% DGT)
  Salary <- as.numeric(temp.salary)
  temp.jobst <- str_extract(text3[[i]], pattern = temp.salary %R% one_or_more(SPC) %R% char_class("A-Z") %R% char_class("A-Z") %R% SPC)
  JobSt <- str_trim(str_extract(temp.jobst, pattern = SPC %R% char_class("A-Z") %R% char_class("A-Z") %R% SPC))
  temp.bunit <- str_extract(text3[[i]], pattern = JobSt %R% one_or_more(SPC) %R% one_or_more(WRD) %R% optional(or(SPC, "-")) 
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC))
  temp.bunit <- str_extract(temp.bunit, one_or_more(SPC) %R% one_or_more(WRD) %R% optional(or(SPC, "-")) 
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)) 
  Bargaining_Unit <- str_trim(temp.bunit)

  public_info[i,1] <- CMP
  public_info[i,2] <- Name
  public_info[i,3] <- Department
  public_info[i,4] <- Title
  public_info[i,5] <- Salary
  public_info[i,6] <- JobSt
  public_info[i,7] <- Bargaining_Unit

}

public_info[1258, 2] <- "De La Cruz, Gabriel R."
public_info[1259, 2] <- "De Urioste-Stone, Sandra M."
public_info[1388, 2] <- "Esparza-St Louis, Deborah"
public_info[2013, 2] <- "Mahoney-O'Neil, Maryellen"
public_info[2308, 2] <- "Pereira Da Cunha, Mauricio"
public_info[2632, 2] <- "St. Louis, Geoffrey A."
public_info[2633, 2] <- "St. Peter, Pamela A."
public_info[3142, 2] <- "De La Garza, Mario A."
public_info[3600, 2] <- "O'Neil Jr, Richard J."
public_info[3681, 2] <- "Raymond Jr., Robert J."
public_info[4407, 2] <- "St. Michel, Peter"
public_info[4408, 2] <- "St. Peter, John A."
public_info[88, 7] <- "COLT"
public_info[15, 3] <- "ABFSP"
public_info[15, 4] <- "Associate Professor - AY"
public_info[1062, 3] <- "OW"
public_info[1062, 4] <- "Administrative Support Supvsr"
public_info[2737, 3] <- "OFM"
public_info[2737, 4] <- "Mech Specialist Mechanical CL1"
public_info[1857, 3] <- "OSBE"
public_info[1857, 4] <- "Research Associate"
public_info[1858, 4] <- "4-H Science Youth Dev Prof"
public_info[1970, 3] <- "OW"
public_info[1970, 4] <- "Postdoctoral Research Assoc"
public_info[1986, 3] <- "OLY"
public_info[1986, 4] <- "Manager of Circulation"
public_info[2183, 3] <- "OHOUS"
public_info[2183, 4] <- "Facilities Maint Worker CL2"
public_info[2536, 3] <- "OFM"
public_info[2536, 4] <- "Electrical Specialist CL2"
public_info[2772, 3] <- "OHOUS"
public_info[2772, 4] <- "Facilities Maint Worker CL1"
public_info[3027, 3] <- "PMUS"
public_info[3027, 4] <- "Assistant Professor of Music"
public_info[3525, 3] <- "PENG"
public_info[3525, 4] <- "Professor of English"
public_info[3696, 3] <- "PCUST"
public_info[3696, 4] <- "Facilities Maint Worker CL1"
public_info[3730, 3] <- "PLYA"
public_info[3730, 4] <- "Coordinator of Access Services"
public_info[4176, 3] <- "SITCSUMF"
public_info[4176, 4] <- "Technical Lead"
public_info[4197, 3] <- "SITINF"
public_info[4197, 4] <- "Adv Comp Cloud Sys Admin"
public_info[4317, 2] <- "Moszczenski III, Stanley"
public_info[4317, 3] <- "SITCSUMA"
public_info[4317, 4] <- "Mgr of Svc Mgmt & Comm"


for (i in 1:length(text3)) {
  temp <- str_extract(public_info$Bargaining_Unit[i], pattern = START %R% one_or_more(WRD) %R% optional(or(SPC, "-")) 
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) %R% optional(SPC)
            %R% optional(one_or_more(WRD)) %R% optional(SPC) %R% optional(one_or_more(WRD)) 
            %R% optional(one_or_more(WRD)))
  public_info[i, 7] <- str_trim(temp)
}

replace <- which(str_detect(public_info$Bargaining_Unit, "University"))
for (i in 1:length(replace)) {
  public_info[replace[i], 7] <- "University Supervisors"
}

replace <- which(str_detect(public_info$Bargaining_Unit, "Part-Time"))
for (i in 1:length(replace)) {
  public_info[replace[i], 7] <- "Part-Time Faculty"
}

replace <- which(str_detect(public_info$Bargaining_Unit, "Service and Maintenance"))
for (i in 1:length(replace)) {
  public_info[replace[i], 7] <- "Service and Maintenance"
}

rm(text2, text, text3, Title, temp.vector, temp.title, temp.salary, temp.name, temp.jobst,
   temp.bunit, temp, Salary, replace, Name, JobSt, i, Department, CMP, Bargaining_Unit)

save(public_info, file = "public_info.RData")

