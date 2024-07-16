CREATE TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ();

ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN acceptance_ratio_weightage int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN acceptance_ratio_window_option json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN actual_pickup_distance_weightage int NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN availability_time_weightage int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN availability_time_window_option json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN cancellation_and_ride_frequency_ratio_window_option json NOT NULL default '{"period":7, "periodType":"Days"}';
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN cancellation_ratio_weightage int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN default_driver_speed double precision NOT NULL default 27.0;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN driver_speed_weightage int NOT NULL default 5;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN intelligent_pool_percentage int ;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN location_update_sample_time int NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN max_num_rides int NOT NULL default 336;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN min_location_updates int NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN min_quotes_to_qualify_for_intelligent_pool int NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN min_quotes_to_qualify_for_intelligent_pool_window_option json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN num_rides_weightage int NOT NULL default 2;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN speed_normalizer double precision NOT NULL default 28;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD PRIMARY KEY ( merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config ADD COLUMN actual_pickup_duration_weightage integer NOT NULL default 0;