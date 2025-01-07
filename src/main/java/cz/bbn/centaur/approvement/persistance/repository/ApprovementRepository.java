package cz.bbn.cerberus.approvement.persistance.repository;

import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Optional;

public interface ApprovementRepository extends JpaRepository<ApprovementEntity, Long>, JpaSpecificationExecutor<ApprovementEntity> {

    @Query( "select entity from ApprovementEntity entity " +
            "left join fetch entity.approvementProjectEmployeeEntity " +
            "join fetch entity.superiorUserEntity " +
            "left join fetch entity.lineManagerUserEntity " +
            "left join fetch entity.lineManagerRoleEntity " +
            "where entity.id = :id")
    Optional<ApprovementEntity> getApprovementEntity(Long id);


    @Modifying
    @Query("update ApprovementEntity entity set entity.superiorUserEntity.id = :userId " +
            "where entity.id = :id")
    void updateSuperior(Long userId, long id);

    @Modifying
    @Query("update ApprovementEntity entity set entity.approvementState = :state " +
            "where entity.id = :id")
    void updateState(ApprovementState state, long id);
}
