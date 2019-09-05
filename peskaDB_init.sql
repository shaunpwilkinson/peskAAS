-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='ONLY_FULL_GROUP_BY,STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

-- -----------------------------------------------------
-- Schema mydb
-- -----------------------------------------------------
-- -----------------------------------------------------
-- Schema wildrlab_peskaDB
-- -----------------------------------------------------
DROP SCHEMA IF EXISTS `wildrlab_peskaDB` ;

-- -----------------------------------------------------
-- Schema wildrlab_peskaDB
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `wildrlab_peskaDB` DEFAULT CHARACTER SET latin1 ;
USE `wildrlab_peskaDB` ;

-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`boat_types`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`boat_types` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`boat_types` (
  `boat_code` INT NOT NULL,
  `boat_description` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`boat_code`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`boats`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`boats` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`boats` (
  `boat_id` INT NOT NULL,
  `boat_name` TEXT NULL DEFAULT NULL,
  `IMEI` TEXT NULL DEFAULT NULL,
  `owner` TEXT NULL DEFAULT NULL,
  `boat_description` TEXT NULL DEFAULT NULL,
  `boat_code` INT NULL DEFAULT NULL,
  `length_m` DOUBLE NULL DEFAULT NULL,
  `motor_type` TEXT NULL DEFAULT NULL,
  `motor_make` TEXT NULL DEFAULT NULL,
  `motor_HP` DOUBLE NULL DEFAULT NULL,
  `VTS_location` TEXT NULL DEFAULT NULL,
  `VTS_covered_when_parked` TEXT NULL DEFAULT NULL,
  `municipality_name` TEXT NULL DEFAULT NULL,
  `community` TEXT NULL DEFAULT NULL,
  `installation_date` TEXT NULL DEFAULT NULL,
  `primary_gear` TEXT NULL DEFAULT NULL,
  `secondary_gear` TEXT NULL DEFAULT NULL,
  `network_provider` TEXT NULL DEFAULT NULL,
  `boat_reg_no` TEXT NULL DEFAULT NULL,
  `notes` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`boat_id`),
  UNIQUE INDEX `boat_id_UNIQUE` (`boat_id` ASC),
  INDEX `fk_boats_1_idx` (`boat_code` ASC),
  CONSTRAINT `fk_boats_1`
    FOREIGN KEY (`boat_code`)
    REFERENCES `wildrlab_peskaDB`.`boat_types` (`boat_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`gear_types`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`gear_types` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`gear_types` (
  `gear_code` INT NOT NULL,
  `gear` TEXT NULL DEFAULT NULL,
  `gear_name` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`gear_code`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`habitat_types`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`habitat_types` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`habitat_types` (
  `habitat_code` INT NOT NULL,
  `English` TEXT NULL DEFAULT NULL,
  `Tetun` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`habitat_code`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`flags`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`flags` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`flags` (
  `flag_code` INT NOT NULL,
  `reason` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`flag_code`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`municipalities`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`municipalities` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`municipalities` (
  `municipality_code` INT NOT NULL,
  `municipality_name` TEXT NULL DEFAULT NULL,
  `fishers` INT NULL DEFAULT NULL,
  `canoes` INT NULL DEFAULT NULL,
  `motors` INT NULL DEFAULT NULL,
  `total_PDS_units` INT NULL DEFAULT NULL,
  `notes` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`municipality_code`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`stations`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`stations` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`stations` (
  `station_code` INT NOT NULL,
  `station_name` TEXT NULL DEFAULT NULL,
  `municipality_code` INT NULL DEFAULT NULL,
  `municipality_name` TEXT NULL DEFAULT NULL,
  `responsibility` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`station_code`),
  INDEX `fk_stations_1_idx` (`municipality_code` ASC),
  CONSTRAINT `fk_stations_1`
    FOREIGN KEY (`municipality_code`)
    REFERENCES `wildrlab_peskaDB`.`municipalities` (`municipality_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`rankings`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`rankings` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`rankings` (
  `rank_code` INT NOT NULL,
  `rank_description` TEXT NULL DEFAULT NULL,
  PRIMARY KEY (`rank_code`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`trips`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`trips` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`trips` (
  `trip_id` INT NOT NULL,
  `date` TEXT NULL DEFAULT NULL,
  `station_code` INT NULL DEFAULT NULL,
  `habitat_code` INT NULL DEFAULT NULL,
  `boat_code` INT NULL DEFAULT NULL,
  `has_PDS` INT NULL DEFAULT NULL,
  `IMEI` TEXT NULL DEFAULT NULL,
  `boat_id` INT NULL DEFAULT NULL,
  `boat_reg_no` TEXT NULL DEFAULT NULL,
  `owner` TEXT NULL DEFAULT NULL,
  `gear_code` INT NULL DEFAULT NULL,
  `mesh` DOUBLE NULL DEFAULT NULL,
  `trip_hours` INT NULL DEFAULT NULL,
  `men` INT NULL DEFAULT NULL,
  `women` INT NULL DEFAULT NULL,
  `children` INT NULL DEFAULT NULL,
  `rel_effort` INT NULL DEFAULT NULL,
  `trip_effort` DOUBLE NULL DEFAULT NULL,
  `catch_value` DOUBLE NULL DEFAULT NULL,
  `rank_code` INT NULL DEFAULT NULL,
  `all_boats` INT NULL DEFAULT NULL,
  `all_gleaners` INT NULL DEFAULT NULL,
  `flag_code` INT NULL DEFAULT NULL,
  `note` TEXT NULL DEFAULT NULL,
  `date_numeric` DOUBLE NULL DEFAULT NULL,
  PRIMARY KEY (`trip_id`),
  INDEX `fk_trips_1_idx` (`boat_code` ASC),
  INDEX `fk_trips_3_idx` (`flag_code` ASC),
  INDEX `fk_trips_4_idx` (`gear_code` ASC),
  INDEX `fk_trips_5_idx` (`habitat_code` ASC),
  INDEX `fk_trips_6_idx` (`station_code` ASC),
  INDEX `fk_trips_7_idx` (`rank_code` ASC),
  CONSTRAINT `fk_trips_1`
    FOREIGN KEY (`boat_code`)
    REFERENCES `wildrlab_peskaDB`.`boat_types` (`boat_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_trips_3`
    FOREIGN KEY (`flag_code`)
    REFERENCES `wildrlab_peskaDB`.`flags` (`flag_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_trips_4`
    FOREIGN KEY (`gear_code`)
    REFERENCES `wildrlab_peskaDB`.`gear_types` (`gear_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_trips_5`
    FOREIGN KEY (`habitat_code`)
    REFERENCES `wildrlab_peskaDB`.`habitat_types` (`habitat_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_trips_6`
    FOREIGN KEY (`station_code`)
    REFERENCES `wildrlab_peskaDB`.`stations` (`station_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_trips_7`
    FOREIGN KEY (`rank_code`)
    REFERENCES `wildrlab_peskaDB`.`rankings` (`rank_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`PDS_trips`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`PDS_trips` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`PDS_trips` (
  `PDS_trip` INT NOT NULL,
  `date` TEXT NULL DEFAULT NULL,
  `boat_id` INT NULL DEFAULT NULL,
  `date_numeric` DOUBLE NULL DEFAULT NULL,
  `IMEI` TEXT NULL DEFAULT NULL,
  `boat_code` INT NULL DEFAULT NULL,
  `station_code` INT NULL DEFAULT NULL,
  `trip_id` INT NULL DEFAULT NULL,
  `gear_code` INT NULL DEFAULT NULL,
  `habitat_code` INT NULL DEFAULT NULL,
  `donor_tripid` INT NULL DEFAULT NULL,
  `donor_distance` DOUBLE NULL DEFAULT NULL,
  `lat1` DOUBLE NULL DEFAULT NULL,
  `lat2` DOUBLE NULL DEFAULT NULL,
  `lat3` DOUBLE NULL DEFAULT NULL,
  `lat4` DOUBLE NULL DEFAULT NULL,
  `lat5` DOUBLE NULL DEFAULT NULL,
  `lat6` DOUBLE NULL DEFAULT NULL,
  `lat7` DOUBLE NULL DEFAULT NULL,
  `lat8` DOUBLE NULL DEFAULT NULL,
  `lat9` DOUBLE NULL DEFAULT NULL,
  `lat10` DOUBLE NULL DEFAULT NULL,
  `lng1` DOUBLE NULL DEFAULT NULL,
  `lng2` DOUBLE NULL DEFAULT NULL,
  `lng3` DOUBLE NULL DEFAULT NULL,
  `lng4` DOUBLE NULL DEFAULT NULL,
  `lng5` DOUBLE NULL DEFAULT NULL,
  `lng6` DOUBLE NULL DEFAULT NULL,
  `lng7` DOUBLE NULL DEFAULT NULL,
  `lng8` DOUBLE NULL DEFAULT NULL,
  `lng9` DOUBLE NULL DEFAULT NULL,
  `lng10` DOUBLE NULL DEFAULT NULL,
  PRIMARY KEY (`PDS_trip`),
  INDEX `fk_PDS_trips_1_idx` (`boat_code` ASC),
  INDEX `fk_PDS_trips_2_idx` (`boat_id` ASC),
  INDEX `fk_PDS_trips_4_idx` (`habitat_code` ASC),
  INDEX `fk_PDS_trips_6_idx` (`station_code` ASC),
  INDEX `fk_PDS_trips_5_idx` (`trip_id` ASC),
  INDEX `fk_PDS_trips_3_idx` (`gear_code` ASC),
  INDEX `fk_PDS_trips_7_idx` (`donor_tripid` ASC),
  CONSTRAINT `fk_PDS_trips_1`
    FOREIGN KEY (`boat_code`)
    REFERENCES `wildrlab_peskaDB`.`boat_types` (`boat_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_PDS_trips_2`
    FOREIGN KEY (`boat_id`)
    REFERENCES `wildrlab_peskaDB`.`boats` (`boat_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_PDS_trips_3`
    FOREIGN KEY (`gear_code`)
    REFERENCES `wildrlab_peskaDB`.`gear_types` (`gear_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_PDS_trips_4`
    FOREIGN KEY (`habitat_code`)
    REFERENCES `wildrlab_peskaDB`.`habitat_types` (`habitat_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_PDS_trips_5`
    FOREIGN KEY (`trip_id`)
    REFERENCES `wildrlab_peskaDB`.`trips` (`trip_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_PDS_trips_6`
    FOREIGN KEY (`station_code`)
    REFERENCES `wildrlab_peskaDB`.`stations` (`station_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_PDS_trips_7`
    FOREIGN KEY (`donor_tripid`)
    REFERENCES `wildrlab_peskaDB`.`trips` (`trip_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`PDS_points`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`PDS_points` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`PDS_points` (
  `PDS_trip` INT NOT NULL,
  `latlng` TEXT NULL DEFAULT NULL,
  CONSTRAINT `fk_PDS_points_1`
    FOREIGN KEY (`PDS_trip`)
    REFERENCES `wildrlab_peskaDB`.`PDS_trips` (`PDS_trip`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`VACs`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`VACs` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`VACs` (
  `vac_code` INT NOT NULL,
  `boat_code` INT NULL DEFAULT NULL,
  `VAC` DOUBLE NULL DEFAULT NULL,
  PRIMARY KEY (`vac_code`),
  INDEX `fk_VACs_1_idx` (`boat_code` ASC),
  CONSTRAINT `fk_VACs_1`
    FOREIGN KEY (`boat_code`)
    REFERENCES `wildrlab_peskaDB`.`boat_types` (`boat_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`species`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`species` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`species` (
  `species_code` INT NOT NULL,
  `fishbase` TEXT NULL DEFAULT NULL,
  `category` TEXT NULL DEFAULT NULL,
  `category_tetun` TEXT NULL DEFAULT NULL,
  `family` TEXT NULL DEFAULT NULL,
  `a` DOUBLE NULL DEFAULT NULL,
  `b` DOUBLE NULL DEFAULT NULL,
  `minlength` INT NULL DEFAULT NULL,
  `maxlength` INT NULL DEFAULT NULL,
  `nstudies` INT NULL DEFAULT NULL,
  `maxweight` DOUBLE NULL DEFAULT NULL,
  PRIMARY KEY (`species_code`))
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


-- -----------------------------------------------------
-- Table `wildrlab_peskaDB`.`landings`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `wildrlab_peskaDB`.`landings` ;

CREATE TABLE IF NOT EXISTS `wildrlab_peskaDB`.`landings` (
  `trip_id` INT NOT NULL,
  `species_code` INT NULL DEFAULT NULL,
  `length` DOUBLE NULL DEFAULT NULL,
  `nfish` INT NULL DEFAULT NULL,
  `weight_g` DOUBLE NULL DEFAULT NULL,
  `food_sale` TEXT NULL DEFAULT NULL,
  `flag_code` INT NULL DEFAULT NULL,
  `note` TEXT NULL DEFAULT NULL,
  INDEX `fk_landings_1_idx` (`trip_id` ASC),
  INDEX `fk_landings_2_idx` (`species_code` ASC),
  INDEX `fk_landings_3_idx` (`flag_code` ASC),
  CONSTRAINT `fk_landings_1`
    FOREIGN KEY (`trip_id`)
    REFERENCES `wildrlab_peskaDB`.`trips` (`trip_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_landings_2`
    FOREIGN KEY (`species_code`)
    REFERENCES `wildrlab_peskaDB`.`species` (`species_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_landings_3`
    FOREIGN KEY (`flag_code`)
    REFERENCES `wildrlab_peskaDB`.`flags` (`flag_code`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB
DEFAULT CHARACTER SET = latin1;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
