package cz.bbn.cerberus.task.persitance.repository;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.task.persitance.entity.TaskEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDateTime;
import java.util.Set;


public interface TaskRepository extends JpaRepository<TaskEntity, Long>, JpaSpecificationExecutor<TaskEntity> {

    int countByObjectTypeAndObjectId(ObjectType objectType, String id);

    int countBySubjectId(String subjectId);

    @Query("select entity from TaskEntity entity " +
            "where entity.userEntity.id in (:userIdSet) and entity.date >= CURRENT_TIMESTAMP " +
            "and (entity.deleted = false or entity.deleted = NULL)")
    Set<TaskEntity> findByUserIdSet(Set<Long> userIdSet);

    @Query("select entity from TaskEntity entity " +
            "where entity.userEntity.id = :userId and entity.date >= :start and entity.date <= :end " +
            "and (entity.deleted = false or entity.deleted = NULL)")
    Set<TaskEntity> findByTaskUserEntitySetIdUserEntity(Long userId, LocalDateTime start, LocalDateTime end);

    @Query("select count(entity.id) from TaskEntity entity " +
            "where entity.userEntity.id = :userId " +
            "and entity.date between :from AND :to and (entity.deleted = false or entity.deleted = NULL)")
    long countByUserAndDate(Long userId, LocalDateTime from, LocalDateTime to);

    @Query("select count(entity.id) from TaskEntity entity " +
            "join entity.taskUserEntitySet taskUserEntity " +
            "where taskUserEntity.id.userEntity.id = :userId " +
            "and entity.date between :from AND :to and (entity.deleted = false or entity.deleted = NULL)")
    long countByTaskUserEntityAndDate(Long userId, LocalDateTime from, LocalDateTime to);
}
