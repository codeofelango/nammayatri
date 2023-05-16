ALTER TABLE atlas_app.person ADD COLUMN is_simulated Bool;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_simulated_routes Text default 'OSRM';
ALTER TABLE atlas_app.person ADD COLUMN action_taken_at timestamp with time zone;
ALTER TABLE atlas_app.person ADD COLUMN action_rule_id Text;
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN get_simulated_distance Text default 'OSRM';

ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_cancelled_by_driver_count_threshold int NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_cancelled_by_driver_count_window json NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_cancellation_count_threshold int NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_cancellation_count_window json NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_total_count_threshold int NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_booking_detection_window json NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_search_count_threshold int NOT NULL;
ALTER TABLE atlas_app.merchant_config ADD COLUMN simulated_search_count_window json NOT NULL;

ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_booking_cancelled_by_driver_count_threshold;
ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_booking_cancelled_by_driver_count_window
ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_booking_cancellation_count_threshold;
ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_booking_cancellation_count_window
ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_booking_total_count_threshold;
ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_booking_detection_window
ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_search_count_threshold;
ALTER TABLE atlas_app.merchant_config DROP COLUMN Simulation_search_count_window