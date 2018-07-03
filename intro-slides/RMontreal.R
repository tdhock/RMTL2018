works_with_R("3.5.0", data.table="1.11.2", ggmap="2.6.1")
rmontreal <- fread("RMontreal.csv", drop=c(7, 9:14))
rmontreal[, table(StatutInscrit_Inscription)]
geocode.list <- if(file.exists("geocode.list.rds")){
  readRDS("geocode.list.rds")
}else{
  list()
}
geocode.dt.list <- list()
for(person.i in 1:nrow(rmontreal)){
  person <- rmontreal[person.i]
  query <- person[["Contact|NumContact::AdresseCorrespondanceComplete"]]
  if(is.na(geocode.list[[query]][["lon"]]) || !query %in% names(geocode.list)){
    cat(sprintf("geocoding %4d / %4d %s\n", person.i, nrow(rmontreal), query))
    g.result <- geocode(query, source="google", output="more")
    Sys.sleep(1)
    geocode.list[[query]] <- g.result
  }
  r <- data.table(
    query,
    geocode.list[[query]][, c("lon", "lat", "country", "address", "administrative_area_level_1")])
  if(grepl("niger", r$address))stop(query)
  if(grepl("usa", r$address))stop(query)
  geocode.dt.list[[person.i]] <- r
}
geocode.dt <- do.call(rbind, geocode.dt.list)
saveRDS(geocode.list, "geocode.list.rds")

geocode.dt[! administrative_area_level_1 %in% c("Québec", "Quebec")]

## ecole secondaire!
rmontreal[grepl("condaire", `Contact|NumContact::AdresseAffiliationComplete`)]

## manitoba
rmontreal[grepl("MB", `Contact|NumContact::AdresseAffiliationComplete`)]


##join.dt <- geocode.dt[rmontreal, on=list(query=`Contact|NumContact::AdresseCorrespondanceComplete`)]
join.dt <- data.table(geocode.dt, rmontreal)
join.dt[country != "Canada"]
join.dt[! administrative_area_level_1 %in% c("Québec", "Quebec")][order(administrative_area_level_1, lat)]

mtl.map <- get_map("montreal, canada")

join.dt[, region := ifelse(
  -74 < lon & lon < -73.25 & 45.2 < lat & lat < 45.8, "Montreal", ifelse(
                                                                  -77 < lon & lon < -50, "QC", ifelse(
    -100 & lon & lon < -50 & 40 < lat & lat < 60, "Canada", "elsewhere")))]
near.mtl <- join.dt[region == "Montreal"]
gg.mtl <- ggmap(mtl.map, extent = "normal")+
  ggtitle(paste(
    nrow(near.mtl),
    "participants from the Montreal area"))+
  ##coord_cartesian(xlim=c(-74, -73.25), ylim=c(45.2, 45.8))+
  geom_point(aes(
    lon, lat),
    data=near.mtl,
    shape=1, color="red")
gg.mtl

pdf("figure-Montreal.pdf")
print(gg.mtl)
dev.off()

qc.map <- get_map("montreal, canada", zoom=7)

near.qc <- join.dt[region %in% c("Montreal", "QC")]
gg.qc <- ggmap(qc.map, extent = "normal")+
  ggtitle(paste(
    nrow(near.qc),
    "participants in or near the province of Quebec"))+
  ##coord_cartesian(xlim=c(-74, -73.25), ylim=c(45.2, 45.8))+
  geom_point(aes(
    lon, lat),
    data=near.qc,
    shape=1, color="red")
gg.qc

pdf("figure-Quebec.pdf")
print(gg.qc)
dev.off()


can.df <- get_map("quebec", zoom=4)

gg.can <- ggmap(can.df)+
  geom_point(aes(lon, lat), data=geocode.dt, shape=1, color="red")
print(gg.can)

pdf("figure-Canada.pdf")
print(gg.can)
dev.off()

euro.df <- get_map("europe", zoom=4)

gg.euro <- ggmap(euro.df)+
  geom_point(aes(lon, lat), data=geocode.dt, shape=1, color="red")
print(gg.euro)

pdf("figure-Europe.pdf")
print(gg.euro)
dev.off()
