-- MariaDB dump 10.19  Distrib 10.5.16-MariaDB, for Linux (x86_64)
--
-- Host: 127.0.0.1    Database: mydb_one
-- ------------------------------------------------------
-- Server version	10.8.3-MariaDB-1:10.8.3+maria~jammy

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `data_properties`
--

DROP TABLE IF EXISTS `data_properties`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_properties` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `prop_key_name` varchar(128) NOT NULL,
  `prop_key` varchar(64) NOT NULL,
  `prop_value` varchar(64) NOT NULL,
  `prop_value_type` enum('STRING','NUMBER') NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_properties`
--

LOCK TABLES `data_properties` WRITE;
/*!40000 ALTER TABLE `data_properties` DISABLE KEYS */;
/*!40000 ALTER TABLE `data_properties` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sites`
--

DROP TABLE IF EXISTS `sites`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sites` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `site_name` varchar(512) NOT NULL,
  `created_at` datetime NOT NULL DEFAULT current_timestamp(),
  `updated_at` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sites`
--

LOCK TABLES `sites` WRITE;
/*!40000 ALTER TABLE `sites` DISABLE KEYS */;
/*!40000 ALTER TABLE `sites` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `voucher_categories`
--

DROP TABLE IF EXISTS `voucher_categories`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `voucher_categories` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `category_name` varchar(256) NOT NULL,
  `price_basic` varchar(100) DEFAULT NULL,
  `duration_value` int(11) NOT NULL,
  `duration_unit` varchar(8) NOT NULL,
  `created_at` datetime NOT NULL DEFAULT current_timestamp(),
  `updated_at` datetime DEFAULT NULL,
  `created_by` varchar(64) NOT NULL,
  `updated_by` varchar(64) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `voucher_categories`
--

LOCK TABLES `voucher_categories` WRITE;
/*!40000 ALTER TABLE `voucher_categories` DISABLE KEYS */;
/*!40000 ALTER TABLE `voucher_categories` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `voucher_category_data_properties`
--

DROP TABLE IF EXISTS `voucher_category_data_properties`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `voucher_category_data_properties` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `voucher_category_id` int(11) NOT NULL,
  `data_property_id` int(11) NOT NULL,
  `prop_key_name` varchar(128) NOT NULL,
  `prop_key` varchar(64) NOT NULL,
  `prop_value` varchar(64) NOT NULL,
  `prop_value_type` enum('STRING','NUMBER') NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `voucher_category_data_properties`
--

LOCK TABLES `voucher_category_data_properties` WRITE;
/*!40000 ALTER TABLE `voucher_category_data_properties` DISABLE KEYS */;
/*!40000 ALTER TABLE `voucher_category_data_properties` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `voucher_usages`
--

DROP TABLE IF EXISTS `voucher_usages`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `voucher_usages` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `voucher_code` varchar(16) NOT NULL,
  `voucher_category_id` int(11) NOT NULL,
  `site_id` int(11) NOT NULL,
  `start_at` datetime NOT NULL,
  `end_at` datetime NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `voucher_usages_UN` (`voucher_code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `voucher_usages`
--

LOCK TABLES `voucher_usages` WRITE;
/*!40000 ALTER TABLE `voucher_usages` DISABLE KEYS */;
/*!40000 ALTER TABLE `voucher_usages` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vouchers`
--

DROP TABLE IF EXISTS `vouchers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vouchers` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `voucher_category_id` int(11) NOT NULL,
  `voucher_code` varchar(16) NOT NULL,
  `site_id` int(11) NOT NULL,
  `is_sold` tinyint(1) NOT NULL DEFAULT 0,
  `created_at` datetime NOT NULL DEFAULT current_timestamp(),
  `deleted_at` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `vouchers_UN` (`voucher_code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vouchers`
--

LOCK TABLES `vouchers` WRITE;
/*!40000 ALTER TABLE `vouchers` DISABLE KEYS */;
/*!40000 ALTER TABLE `vouchers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Dumping routines for database 'mydb_one'
--
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2022-08-23 20:35:33
