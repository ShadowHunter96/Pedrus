package cz.bbn.cerberus.attendance.persistance.repository;

import cz.bbn.cerberus.attendance.persistance.entity.AttendanceReportEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface AttendanceReportRepository extends JpaRepository<AttendanceReportEntity, Long>, JpaSpecificationExecutor<AttendanceReportEntity> {

    @Query("select entity from AttendanceReportEntity entity " +
            "where YEAR(entity.id.date) = :year and MONTH(entity.id.date) = :month " +
            "order by entity.employeeName, entity.id.date")
    List<AttendanceReportEntity> findAttendanceReportEntitySet(int year, int month);
}
