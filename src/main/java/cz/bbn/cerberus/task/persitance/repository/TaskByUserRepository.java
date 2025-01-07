package cz.bbn.cerberus.task.persitance.repository;

import cz.bbn.cerberus.task.persitance.entity.TaskEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskUserEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskUserId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;

public interface TaskByUserRepository extends JpaRepository<TaskUserEntity, TaskUserId> {

    @Modifying
    void deleteByIdTaskEntityId(Long taskId);

    @Query("select entity.id.taskEntity from TaskUserEntity entity " +
            "where entity.id.userEntity.id in (:userIds) and entity.id.taskEntity.date >= CURRENT_TIMESTAMP ")
    Set<TaskEntity> findByUserIdSet(Set<Long> userIds);
}
