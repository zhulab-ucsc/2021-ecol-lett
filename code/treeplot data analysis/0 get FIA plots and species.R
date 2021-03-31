# This script uses postgres database on a local windows machine
library(tidyverse)
library(DBI)
library(maps)

##### Connect to server
con <- dbConnect(RPostgres::Postgres(),
                 host = "localhost",
                 user = "postgres",
                 password = "pw",
                 dbname = "testdb"
)

# list tables
dbGetQuery(
  con,
  "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='fs_fiadb'"
)

# get all plots
plot_df <- dbGetQuery(con, "SELECT * FROM fs_fiadb.plot")
nrow(plot_df)
dbGetQuery(con, "SELECT COUNT(*) FROM fs_fiadb.plot")

##### Get plot tables

# plot back 0 
dbRemoveTable(con, DBI::SQL("song.plot_back0")) 
dbExecute(con, "
CREATE TABLE song.plot_back0 AS
SELECT p.cn AS plt, --cn is sequence number. this is primary key
p.lon, --latitude
p.lat, --longitude
p.elev, --elevation
p.prev_plt_cn, --previous plot sequence number
p.invyr, --inventory year. this is unique key
p.statecd, --state code. this is unique key
p.unitcd, --survey unit code. this is unique key
p.countycd, --county code. this is unique key
p.plot, --plot number. this is unique key
p.measyear, --measurement year
p.measmon, --measurement month
p.measday, --measurement day
p.ecosubcd --ecoregion
--ppsa.evalid
FROM fs_fiadb.plot p, --plot table (location level)
fs_fiadb.cond c, --condition table (location level)
fs_fiadb.pop_plot_stratum_assgn ppsa --population plot stratum assignment table (population) 
WHERE p.cn = c.plt_cn -- fully forest plot --plt_cn is plot sequence number. Foreign key linking CONDITION to PLOT
AND c.cond_status_cd = 1 --cond_status_cd is condition status code. only forest.
AND c.condprop_unadj = 1 --condprop_unadj is condition proportion unadjusted.
AND c.trtcd1 = 00 -- non-treated plots --treatment code 1
AND (c.trtcd2 = 00
     OR c.trtcd2 IS NULL)-- Some plots don't have a zero for all three treatment codes --some stands experienced more than one treatments
  AND (c.trtcd3 = 00
       OR c.trtcd3 IS NULL) 
AND c.dstrbcd1 = 00 -- non-disturbed plots
AND (c.dstrbcd2 = 00
     OR c.dstrbcd2 IS NULL)
  AND (c.dstrbcd3 = 00
       OR c.dstrbcd3 IS NULL) 
  AND p.cn = ppsa.plt_cn 
  AND ppsa.evalid IN --evaluation identifier. The EVALID is the unique identifier that represents the population used to produce a type of estimate.
    (SELECT pe_sub.evalid /* Evalid search code from Brian on 8/3/2018 */
     FROM 
       (SELECT pe.*
        FROM fs_fiadb.pop_eval pe --population evaluation table (population)
        INNER JOIN 
          (SELECT statecd, 
                  location_nm, -- location name 
                  MAX(end_invyr) max_end_invyr  --end inventory year
           FROM fs_fiadb.pop_eval 
           GROUP BY statecd, 
                    location_nm) x ON (pe.statecd = x.statecd
                                       AND pe.location_nm = x.location_nm
                                       AND pe.end_invyr = x.max_end_invyr)
        WHERE (pe.evalid::text LIKE '%01' -- ::text coerce type, which is unique in postgres 
               OR pe.evalid::text LIKE '%02' 
               OR pe.evalid::text LIKE '%21' 
               OR pe.evalid::text LIKE '%22') 
          AND pe.eval_descr::text LIKE '%CURRENT AREA, CURRENT VOLUME%'
        ORDER BY pe.statecd) AS pe_sub)
")

plot_back0 <- dbGetQuery(con, "SELECT * FROM song.plot_back0") 

nrow(plot_back0)
map("state")
points(plot_back0[, c("lon", "lat")])

# back 1
dbRemoveTable(con, DBI::SQL("song.plot_back1")) 
dbExecute(con, "
CREATE TABLE song.plot_back1 AS
SELECT p.cn AS plt,
       p.lon,
       p.lat,
       p.elev,
       p.prev_plt_cn,
       p.invyr,
       p.statecd,
       p.unitcd,
       p.countycd,
       p.plot,
       p.measyear,
       p.measmon,
       p.measday,
       p.ecosubcd
FROM song.plot_back0 r,
     fs_fiadb.plot p,
     fs_fiadb.cond c
WHERE r.prev_plt_cn = p.cn
  AND p.cn = c.plt_cn
  AND c.cond_status_cd = 1
  AND c.condprop_unadj = 1
  AND c.trtcd1 = 00 -- non-treated plots
  AND (c.trtcd2 = 00
       OR c.trtcd2 IS NULL)
  AND (c.trtcd3 = 00
       OR c.trtcd3 IS NULL)
  AND c.dstrbcd1 = 00 -- non-disturbed plots
  AND (c.dstrbcd2 = 00
       OR c.dstrbcd2 IS NULL)
  AND (c.dstrbcd3 = 00
       OR c.dstrbcd3 IS NULL)")

plot_back1 <- dbGetQuery(con, "SELECT * FROM song.plot_back1") 

nrow(plot_back1)
map("state")
points(plot_back1[, c("lon", "lat")])

# back 2
dbRemoveTable(con, DBI::SQL("song.plot_back2")) 
dbExecute(con, "
CREATE TABLE song.plot_back2 AS
SELECT p.cn AS plt,
       p.lon,
       p.lat,
       p.elev,
       p.prev_plt_cn,
       p.invyr,
       p.statecd,
       p.unitcd,
       p.countycd,
       p.plot,
       p.measyear,
       p.measmon,
       p.measday，
       p.ecosubcd
FROM song.plot_back1 r,
     fs_fiadb.plot p,
     fs_fiadb.cond c
WHERE r.prev_plt_cn = p.cn
  AND p.cn = c.plt_cn
  AND c.cond_status_cd = 1
  AND c.condprop_unadj = 1
  AND c.trtcd1 = 00 -- non-treated plots
  AND (c.trtcd2 = 00
       OR c.trtcd2 IS NULL)
  AND (c.trtcd3 = 00
       OR c.trtcd3 IS NULL)
  AND c.dstrbcd1 = 00 -- non-disturbed plots
  AND (c.dstrbcd2 = 00
       OR c.dstrbcd2 IS NULL)
  AND (c.dstrbcd3 = 00
       OR c.dstrbcd3 IS NULL)")

plot_back2 <- dbGetQuery(con, "SELECT * FROM song.plot_back2") 

nrow(plot_back2)
map("state")
points(plot_back2[, c("lon", "lat")])

# back 3
dbRemoveTable(con, DBI::SQL("song.plot_back3")) 
dbExecute(con, "
CREATE TABLE song.plot_back3 AS
SELECT p.cn AS plt,
       p.lon,
       p.lat,
       p.elev,
       p.prev_plt_cn,
       p.invyr,
       p.statecd,
       p.unitcd,
       p.countycd,
       p.plot,
       p.measyear,
       p.measmon,
       p.measday,
       p.ecosubcd
FROM song.plot_back2 r,
     fs_fiadb.plot p,
     fs_fiadb.cond c
WHERE r.prev_plt_cn = p.cn
  AND p.cn = c.plt_cn
  AND c.cond_status_cd = 1
  AND c.condprop_unadj = 1
  AND c.trtcd1 = 00 -- non-treated plots
  AND (c.trtcd2 = 00
       OR c.trtcd2 IS NULL)
  AND (c.trtcd3 = 00
       OR c.trtcd3 IS NULL)
  AND c.dstrbcd1 = 00 -- non-disturbed plots
  AND (c.dstrbcd2 = 00
       OR c.dstrbcd2 IS NULL)
  AND (c.dstrbcd3 = 00
       OR c.dstrbcd3 IS NULL)")

plot_back3 <- dbGetQuery(con, "SELECT * FROM song.plot_back3") 

nrow(plot_back3)
map("state")
points(plot_back3[, c("lon", "lat")])

# back 4
dbRemoveTable(con, DBI::SQL("song.plot_back4")) 
dbExecute(con, "
CREATE TABLE song.plot_back4 AS
SELECT p.cn AS plt,
       p.lon,
       p.lat,
       p.elev,
       p.prev_plt_cn,
       p.invyr,
       p.statecd,
       p.unitcd,
       p.countycd,
       p.plot,
       p.measyear,
       p.measmon,
       p.measday,
       p.ecosubcd
FROM song.plot_back3 r,
     fs_fiadb.plot p,
     fs_fiadb.cond c
WHERE r.prev_plt_cn = p.cn
  AND p.cn = c.plt_cn
  AND c.cond_status_cd = 1
  AND c.condprop_unadj = 1
  AND c.trtcd1 = 00 -- non-treated plots
  AND (c.trtcd2 = 00
       OR c.trtcd2 IS NULL)
  AND (c.trtcd3 = 00
       OR c.trtcd3 IS NULL)
  AND c.dstrbcd1 = 00 -- non-disturbed plots
  AND (c.dstrbcd2 = 00
       OR c.dstrbcd2 IS NULL)
  AND (c.dstrbcd3 = 00
       OR c.dstrbcd3 IS NULL)")

plot_back4 <- dbGetQuery(con, "SELECT * FROM song.plot_back4") 

nrow(plot_back4)
map("state")
points(plot_back4[, c("lon", "lat")])

##### Get species tables

# back 0
dbRemoveTable(con, DBI::SQL("song.species_back0"))
dbExecute(con, "
CREATE TABLE song.species_back0 AS
SELECT r.plt,
       r.lon,
       r.lat,
       r.elev,
	     r.invyr,
       r.statecd,
       r.unitcd,
       r.countycd,
       r.plot,
       r.measyear,
       r.measmon,
       r.measday,
       r.ecosubcd,
       ts.tree,
       ts.treeinplt,
       ts.spcd,
       ts.dia,
       ts.common_name,
       ts.genus,
       ts.species
FROM song.plot_back0 r,
     (SELECT t.plt_cn,
             t.cn AS tree,
             t.tree AS treeinplt,
             t.spcd,
             t.dia,
             s.common_name,
             s.genus,
             s.species
      FROM fs_fiadb.tree t
      LEFT JOIN fs_fiadb.ref_species s
      ON s.spcd=t.spcd) AS ts
WHERE r.plt = ts.plt_cn
")

species_back0 <- dbGetQuery(con, "SELECT * FROM song.species_back0") 

nrow(species_back0)

# view some data
species_back0<-dbReadTable(con, DBI::SQL("song.species_back0"))

summary_df<-species_back0 %>%
  group_by(spcd) %>%
  summarize(median=median(elev,na.rm=T))
head(summary_df)

species_back0_subset<-species_back0 %>% 
  filter(spcd<=20) %>% 
  mutate(spcd=as.character(spcd))
ggplot(species_back0_subset,aes(x=spcd,y=elev))+
  geom_boxplot()

# back 1
dbRemoveTable(con, DBI::SQL("song.species_back1"))
dbExecute(con, "
CREATE TABLE song.species_back1 AS
SELECT r.plt,
       r.lon,
       r.lat,
       r.elev,
	     r.invyr,
       r.statecd,
       r.unitcd,
       r.countycd,
       r.plot,
       r.measyear,
       r.measmon,
       r.measday,
       r.ecosubcd,
       ts.tree,
       ts.treeinplt,
       ts.spcd,
       ts.dia,
       ts.common_name,
       ts.genus,
       ts.species
FROM song.plot_back1 r,
     (SELECT t.plt_cn,
             t.cn AS tree,
             t.tree AS treeinplt,
             t.spcd,
             t.dia,
             s.common_name,
             s.genus,
             s.species
      FROM fs_fiadb.tree t
      LEFT JOIN fs_fiadb.ref_species s
      ON s.spcd=t.spcd) AS ts
WHERE r.plt = ts.plt_cn
")

species_back1 <- dbGetQuery(con, "SELECT * FROM song.species_back1") 

nrow(species_back1)

# back 2
dbRemoveTable(con, DBI::SQL("song.species_back2"))
dbExecute(con, "
CREATE TABLE song.species_back2 AS
SELECT r.plt,
       r.lon,
       r.lat,
       r.elev,
	     r.invyr,
       r.statecd,
       r.unitcd,
       r.countycd,
       r.plot,
       r.measyear,
       r.measmon,
       r.measday,
       r.ecosubcd,
       ts.tree,
       ts.treeinplt,
       ts.spcd,
       ts.dia,
       ts.common_name,
       ts.genus,
       ts.species
FROM song.plot_back2 r,
     (SELECT t.plt_cn,
             t.cn AS tree,
             t.tree AS treeinplt,
             t.spcd,
             t.dia,
             s.common_name,
             s.genus,
             s.species
      FROM fs_fiadb.tree t
      LEFT JOIN fs_fiadb.ref_species s
      ON s.spcd=t.spcd) AS ts
WHERE r.plt = ts.plt_cn
")

species_back2 <- dbGetQuery(con, "SELECT * FROM song.species_back2") 

nrow(species_back2)

# back 3
dbRemoveTable(con, DBI::SQL("song.species_back3"))
dbExecute(con, "
CREATE TABLE song.species_back3 AS
SELECT r.plt,
       r.lon,
       r.lat,
       r.elev,
	     r.invyr,
       r.statecd,
       r.unitcd,
       r.countycd,
       r.plot,
       r.measyear,
       r.measmon,
       r.measday,
       r.ecosubcd,
       ts.tree,
       ts.treeinplt,
       ts.spcd,
       ts.dia,
       ts.common_name,
       ts.genus,
       ts.species
FROM song.plot_back3 r,
     (SELECT t.plt_cn,
             t.cn AS tree,
             t.tree AS treeinplt,
             t.spcd,
             t.dia,
             s.common_name,
             s.genus,
             s.species
      FROM fs_fiadb.tree t
      LEFT JOIN fs_fiadb.ref_species s
      ON s.spcd=t.spcd) AS ts
WHERE r.plt = ts.plt_cn
")

species_back3 <- dbGetQuery(con, "SELECT * FROM song.species_back3") 

nrow(species_back3)

# back 4
dbRemoveTable(con, DBI::SQL("song.species_back4"))
dbExecute(con, "
CREATE TABLE song.species_back4 AS
SELECT r.plt,
       r.lon,
       r.lat,
       r.elev,
	     r.invyr,
       r.statecd,
       r.unitcd,
       r.countycd,
       r.plot,
       r.measyear,
       r.measmon,
       r.measday,
       r.ecosubcd,
       ts.tree,
       ts.treeinplt,
       ts.spcd,
       ts.dia,
       ts.common_name,
       ts.genus,
       ts.species
FROM song.plot_back4 r,
     (SELECT t.plt_cn,
             t.cn AS tree,
             t.tree AS treeinplt,
             t.spcd,
             t.dia,
             s.common_name,
             s.genus,
             s.species
      FROM fs_fiadb.tree t
      LEFT JOIN fs_fiadb.ref_species s
      ON s.spcd=t.spcd) AS ts
WHERE r.plt = ts.plt_cn
")

species_back4 <- dbGetQuery(con, "SELECT * FROM song.species_back4") 

nrow(species_back4)

##### Export data

# plot_back0<-dbReadTable(con, DBI::SQL("song.plot_back0"))
# plot_back1<-dbReadTable(con, DBI::SQL("song.plot_back1"))
# plot_back2<-dbReadTable(con, DBI::SQL("song.plot_back2"))
# plot_back3<-dbReadTable(con, DBI::SQL("song.plot_back3"))
# plot_back4<-dbReadTable(con, DBI::SQL("song.plot_back4"))

species_back0<-dbReadTable(con, DBI::SQL("song.species_back0")) %>% 
  mutate(census=0)
species_back1<-dbReadTable(con, DBI::SQL("song.species_back1")) %>% 
  mutate(census=1)
species_back2<-dbReadTable(con, DBI::SQL("song.species_back2")) %>% 
  mutate(census=2)
species_back3<-dbReadTable(con, DBI::SQL("song.species_back3")) %>% 
  mutate(census=3)
species_back4<-dbReadTable(con, DBI::SQL("song.species_back4")) %>% 
  mutate(census=4)

species_df<-rbind(species_back0,species_back1,species_back2,species_back3,species_back4)
write_csv(species_df, gzfile("./data/FIA raw data.csv.gz"))