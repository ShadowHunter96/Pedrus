package cz.bbn.cerberus.workreport.persistance.repository;

import cz.bbn.cerberus.workreport.persistance.entity.WorkReportEntity;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;
import java.util.Set;

public interface WorkReportRepository extends JpaRepository<WorkReportEntity, Long> {

    @Query("select entity from WorkReportEntity entity " +
            "where entity.employeeId = :empId and entity.date between :start and :end")
    List<WorkReportEntity> getPeriodWorkReportDtoList(LocalDate start, LocalDate end, String empId);

    @Query("select entity from WorkReportEntity entity " +
            "where entity.employeeId in :empIdSet and entity.date between :start and :end")
    List<WorkReportEntity> getPeriodWorkReportDtoList(LocalDate start, LocalDate end, Set<String> empIdSet);

    @Query("select entity from WorkReportEntity entity where entity.employeeId = :empId")
    List<WorkReportEntity> getAllWorkReportDtoList(String empId);

    @Modifying
    void deleteByApprovementEntityIdAndEmployeeId(Long approvementId, String employeeId);

    @Query("select entity from WorkReportEntity entity " +
            "where entity.approvementEntity != null " +
            "and entity.date = :date and entity.employeeId = :employeeId ")
    WorkReportEntity getWorkReportWithApprovement(LocalDate date, String employeeId, PageRequest page);
}
