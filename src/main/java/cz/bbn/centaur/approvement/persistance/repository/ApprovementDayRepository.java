package cz.bbn.cerberus.approvement.persistance.repository;

import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementDayEntity;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface ApprovementDayRepository extends JpaRepository<ApprovementDayEntity, Long> {

    @Modifying
    void deleteByApprovementEntityId(Long approvementId);

    @Query("select entity from ApprovementDayEntity entity " +
            "where entity.date between :from and :to " +
            "and entity.approvementEntity.ownerUserEntity.id = :userId " +
            "and entity.approvementEntity.id != :requestId " +
            "and entity.approvementEntity.approvementState not in ('DENIED', 'CANCELED')")
    ApprovementDayEntity getApprovementByDate(LocalDateTime from, LocalDateTime to, Long userId, Long requestId, PageRequest pageRequest);

    @Query("select entity from ApprovementDayEntity entity " +
            "where entity.date between :from and :to " +
            "and entity.approvementEntity.ownerUserEntity.id = :userId " +
            "and entity.approvementEntity.approvementState not in ('DENIED', 'CANCELED')")
    ApprovementDayEntity getApprovementByDate(LocalDateTime from, LocalDateTime to, Long userId, PageRequest pageRequest);


    @Query( "select count(entity.date) from ApprovementDayEntity entity " +
            "where entity.approvementEntity.approvementType = :type " +
            "and YEAR(entity.date) = :year and entity.approvementEntity.halfDay = false " +
            "and entity.approvementEntity.approvementState not in ('DENIED', 'CANCELED')" +
            "and entity.approvementEntity.ownerUserEntity.id = :userId")
    Optional<Double> getCount(ApprovementType type, Integer year, Long userId);

    @Query( "select count(entity.date) from ApprovementDayEntity entity " +
            "where entity.approvementEntity.approvementType = :type " +
            "and YEAR(entity.date) = :year and entity.approvementEntity.halfDay = true " +
            "and entity.approvementEntity.approvementState not in ('DENIED', 'CANCELED')" +
            "and entity.approvementEntity.ownerUserEntity.id = :userId")
    Optional<Double> getCountHalfDays(ApprovementType type, Integer year, Long userId);

    @Query( "select count(entity.date) from ApprovementDayEntity entity " +
            "where entity.approvementEntity.approvementType = :type " +
            "and YEAR(entity.date) = :year " +
            "and entity.approvementEntity.approvementState not in ('DENIED', 'CANCELED')" +
            "and entity.approvementEntity.ownerUserEntity.id = :userId " +
            "and entity.approvementEntity.id != :dtoId")
    Optional<Double> getCount(ApprovementType type, Integer year, Long userId, Long dtoId);


    @Query(" select count(entity.date) from ApprovementDayEntity entity " +
            "where entity.approvementEntity.ownerUserEntity.id = :userId " +
            "and entity.approvementEntity.approvementType in (:approvementTypeList) " +
            "and entity.date = :date and entity.approvementEntity.halfDay != true " +
            "and entity.approvementEntity.approvementState not in ('DENIED', 'CANCELED')")
    int getUserApprovementCount(Long userId, List<ApprovementType> approvementTypeList, LocalDateTime date);

    @Query(" select entity from ApprovementDayEntity entity " +
            "where entity.approvementEntity.ownerUserEntity.id = :userId " +
            "and entity.approvementEntity.approvementType = 'BUSSINES_TRIP' " +
            "and entity.date between :from and :to " +
            "and entity.approvementEntity.approvementState not in ('DENIED', 'CANCELED')")
    List<ApprovementDayEntity> getBusinessTripApprovement(LocalDateTime from, LocalDateTime to, Long userId);
}
