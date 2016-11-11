#' Session count from Avvo hadoop.
#'
#' A dataset containing session counts (visits) by market and page type. Page type is divided into "core" and "others". See SQL for categorization.
#'
#' @format A data.table with 257481 rows and 5 variables
#' @author Jane Wang.
#' @section SQL Code:
#' \preformatted{
#' select cast(concat(cast(year(a.event_date) as string), lpad(cast(month(a.event_date) as string),2,'0')) as int) as year_month
#' , a.ad_market_id
#' , a.state
#' , a.county
#' , a.region
#' , a.specialty_name
#' , a.parent_specialty_name
#' , a.block_flag
#' , case when a.page in ('Attorney_Directory_Browse', 'Attorney_Search','Attorney_Profile','Attorney_Profile_Aboutme','Attorney_Profile_Contact','Attorney_Profile_Endorsement') then 'core'
#' else 'others' end as page
#' --  , b.medium
#' , count(distinct a.session_id) sessions
#' from
#' (
#' select pv.session_id
#' , pv.event_date
#' , pv.page_type as page
#' , amd.ad_market_id
#' , amd.ad_market_state_name as state
#' , amd.ad_market_county_name as county
#' , amd.ad_market_region_name as region
#' , sd.specialty_name
#' , sd.parent_specialty_name
#' , amd.ad_market_block_flag as block_flag
#' from src.page_view pv
#' left join dm.specialty_dimension sd on pv.specialty_id = sd.specialty_id
#' left join dm.geography_dimension gd on gd.geo_id = pv.location_id
#' left join dm.ad_market_dimension amd on amd.specialty_id = pv.specialty_id and amd.ad_region_id = gd.sales_region_id
#' where pv.event_date between '2016-02-01' and '2016-07-31'
#' ) a
#' join
#' (
#' select event_date
#' , session_id
#' , medium
#' from
#' (
#' select event_date
#' , session_id
#' , case when url like '%utm_campaign=brand%' then 'brand'
#' when url like '%utm_campaign=pls%'  then 'pls'
#' when url like '%utm_content=sgt%' then 'network'
#' when url like '%utm_campaign=adblock%' then 'adblock' else 'nonsem' end as medium
#' ,row_number() OVER (PARTITION BY session_id ORDER BY gmt_timestamp ASC) rankNum
#' from src.page_view
#' WHERE session_id IS NOT NULL
#' AND persistent_session_id IS NOT NULL
#' AND render_instance_guid IS NOT NULL
#' and event_date between '2016-02-01' and '2016-07-31'
#' ) c
#' where rankNum=1
#' and medium!='network'
#' ) b on b.event_date = a.event_date and b.session_id = a.session_id
#' where ad_market_id>0 and ad_market_id is not null
#' -- and a.session_id='20a58a68-62e2-4bb6-840f-1361ef96d953.1'
#' group by 1,2,3,4,5,6,7,8,9
#' }
"traffic"
#> [1] "traffic"


#' County level census data
#'
#' Description of the columns exists in an Excel file.
#'
#' \describe{
#'\item{marketkey}{Unique market id. Should really be replaced wby marketid}
#'\item{ppc}{ppc value for the market}
#'
#' }
#'
#' @format A data.table with 3135 rows and 164 variables
#' @author Rahul Dodhia
"ctylevel"
#> [1] "ctylevel"
