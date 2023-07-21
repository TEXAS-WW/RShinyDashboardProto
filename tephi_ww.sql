-- MariaDB dump 10.19  Distrib 10.11.2-MariaDB, for Win64 (AMD64)
--
-- Host: localhost    Database: tephi_ww
-- ------------------------------------------------------
-- Server version	10.11.2-MariaDB

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
-- Table structure for table `genome_coverage`
--

DROP TABLE IF EXISTS `genome_coverage`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `genome_coverage` (
  `Sample_ID` varchar(50) NOT NULL,
  `accession` varchar(100) DEFAULT NULL,
  `start_base` int(11) DEFAULT NULL,
  `end_base` int(11) DEFAULT NULL,
  `mean_depth` double DEFAULT NULL,
  PRIMARY KEY (`Sample_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `genome_coverage`
--

LOCK TABLES `genome_coverage` WRITE;
/*!40000 ALTER TABLE `genome_coverage` DISABLE KEYS */;
/*!40000 ALTER TABLE `genome_coverage` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `metadata`
--

DROP TABLE IF EXISTS `metadata`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `metadata` (
  `SampleID_samplesheet` varchar(100) NOT NULL,
  `Site` varchar(100) DEFAULT NULL,
  `City` varchar(100) DEFAULT NULL,
  `Collection_Date` date DEFAULT NULL,
  `Flow_Rate` double DEFAULT NULL,
  PRIMARY KEY (`SampleID_samplesheet`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `metadata`
--

LOCK TABLES `metadata` WRITE;
/*!40000 ALTER TABLE `metadata` DISABLE KEYS */;
/*!40000 ALTER TABLE `metadata` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qpcr`
--

DROP TABLE IF EXISTS `qpcr`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `qpcr` (
  `LocationAbbr` varchar(50) NOT NULL,
  `CMMR_Barcode` varchar(50) NOT NULL,
  `Target` varchar(50) DEFAULT NULL,
  `Ct` double DEFAULT NULL,
  `copiesperml` double DEFAULT NULL,
  `SampleName` varchar(50) DEFAULT NULL,
  `date_of_collection` date DEFAULT NULL,
  PRIMARY KEY (`CMMR_Barcode`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qpcr`
--

LOCK TABLES `qpcr` WRITE;
/*!40000 ALTER TABLE `qpcr` DISABLE KEYS */;
/*!40000 ALTER TABLE `qpcr` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `site_coding`
--

DROP TABLE IF EXISTS `site_coding`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `site_coding` (
  `Name` varchar(50) NOT NULL,
  `Code` varchar(100) NOT NULL,
  PRIMARY KEY (`Name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `site_coding`
--

LOCK TABLES `site_coding` WRITE;
/*!40000 ALTER TABLE `site_coding` DISABLE KEYS */;
/*!40000 ALTER TABLE `site_coding` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `taxonomical_profiles`
--

DROP TABLE IF EXISTS `taxonomical_profiles`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `taxonomical_profiles` (
  `accession` varchar(100) DEFAULT NULL,
  `reference_length` int(11) DEFAULT NULL,
  `covered_base` int(11) DEFAULT NULL,
  `reads_aligned` int(11) DEFAULT NULL,
  `mean_coverage` double DEFAULT NULL,
  `RPKM` double DEFAULT NULL,
  `sequence_name` varchar(100) DEFAULT NULL,
  `taxid` varchar(100) DEFAULT NULL,
  `kingdom` varchar(100) DEFAULT NULL,
  `phylum` varchar(100) DEFAULT NULL,
  `class_var` varchar(100) DEFAULT NULL,
  `order_var` varchar(100) DEFAULT NULL,
  `family` varchar(100) DEFAULT NULL,
  `genus` varchar(100) DEFAULT NULL,
  `species` varchar(100) DEFAULT NULL,
  `subspecies` varchar(100) DEFAULT NULL,
  `strain` varchar(100) DEFAULT NULL,
  `sample_ID` varchar(100) NOT NULL,
  `seq_pool_ID` varchar(100) DEFAULT NULL,
  `total_filtered_reads_in_sample` double DEFAULT NULL,
  PRIMARY KEY (`sample_ID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `taxonomical_profiles`
--

LOCK TABLES `taxonomical_profiles` WRITE;
/*!40000 ALTER TABLE `taxonomical_profiles` DISABLE KEYS */;
/*!40000 ALTER TABLE `taxonomical_profiles` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `wwtp_location`
--

DROP TABLE IF EXISTS `wwtp_location`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `wwtp_location` (
  `State` varchar(50) DEFAULT NULL,
  `County` varchar(50) DEFAULT NULL,
  `City` varchar(50) DEFAULT NULL,
  `WWTP` varchar(100) NOT NULL,
  `lat` double DEFAULT NULL,
  `lon` double DEFAULT NULL,
  `centroid_lat` double DEFAULT NULL,
  `centroid_lon` double DEFAULT NULL,
  PRIMARY KEY (`WWTP`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `wwtp_location`
--

LOCK TABLES `wwtp_location` WRITE;
/*!40000 ALTER TABLE `wwtp_location` DISABLE KEYS */;
/*!40000 ALTER TABLE `wwtp_location` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2023-07-07  0:05:53
