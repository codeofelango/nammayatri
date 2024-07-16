CREATE TABLE atlas_app.rider_config ();

ALTER TABLE atlas_app.rider_config ADD COLUMN app_url text NOT NULL default 'nammayatri.in/link/rider/rmxw';
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_emergency_contact_added_message boolean NOT NULL default true;
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_local_police_support boolean NOT NULL default false;
ALTER TABLE atlas_app.rider_config ADD COLUMN enable_support_for_safety boolean NOT NULL default false;
ALTER TABLE atlas_app.rider_config ADD COLUMN local_police_number text ;
ALTER TABLE atlas_app.rider_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rider_config ADD COLUMN safety_check_end_time integer NOT NULL default 21600;
ALTER TABLE atlas_app.rider_config ADD COLUMN safety_check_start_time integer NOT NULL default 75600;
ALTER TABLE atlas_app.rider_config ADD COLUMN time_diff_from_utc integer NOT NULL default 19800;
ALTER TABLE atlas_app.rider_config ADD COLUMN tracking_short_url_pattern text NOT NULL default 'nammayatri.in/t/';
ALTER TABLE atlas_app.rider_config ADD COLUMN video_file_size_upper_limit integer NOT NULL default 15000000;
ALTER TABLE atlas_app.rider_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.rider_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rider_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rider_config ADD PRIMARY KEY ( merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN collect_auto_complete_data boolean ;
ALTER TABLE atlas_app.rider_config ADD COLUMN special_zone_radius integer NOT NULL default 150;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN distance_weightage integer NOT NULL default 70;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN collect_mmi_route_data boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN is_avoid_toll boolean NOT NULL default true;
ALTER TABLE atlas_app.rider_config ADD COLUMN auto_unblock_safety_center_after_days integer NOT NULL default 14;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN place_name_cache_expiry_days integer ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN booking_sync_status_call_seconds_diff_threshold integer ;



------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN kapture_queue text NOT NULL default '';


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN email_otp_config json;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN kapture_config json NOT NULL default '{"kaptureQueue":"", "disposition":""}';


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN police_trigger_delay integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN ivr_trigger_delay integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN hard_limit_for_safety_jobs integer ;
ALTER TABLE atlas_app.rider_config ADD COLUMN exotel_app_id_mapping json ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN incident_report_support boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.rider_config ADD COLUMN exotel_status_check_scheduler integer ;