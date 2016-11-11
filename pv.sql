With A as
(
select
pv.page_type as pt
,sd.specialty_name as pa
,count(distinct render_instance_guid) count_pv
from src.page_view pv
left join dm.specialty_dimension sd on pv.specialty_id = sd.specialty_id
left join dm.geography_dimension gd on gd.geo_id = pv.location_id
left join dm.ad_market_dimension amd on amd.specialty_id = pv.specialty_id and amd.ad_region_id = gd.sales_region_id
where pv.event_date between '2016-08-01' and '2016-08-31'
and sd.specialty_name='DUI & DWI'
group by 1,2
)
, B as
(
select sum(sl_adimpression) v3_slimps
,sum(page_view) v3_pv
,ad_page_type pt,ad_market_specialty_name as pa
from dm.webanalytics_ad_attribution_v3 pam
join dm.ad_market_dimension amd on amd.ad_market_id=pam.ad_market_id
where ad_market_specialty_name in ('Divorce & Separation','DUI & DWI','Criminal Defense')
and event_date between '2016-08-01' and '2016-08-31'
group by ad_page_type,ad_market_specialty_name
order by ad_market_specialty_name,ad_page_type
)

select *
  from A join B on A.pa=B.pa and A.pt=B.pt
