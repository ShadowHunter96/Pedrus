package cz.bbn.cerberus.task.persitance.repository;

import cz.bbn.cerberus.task.persitance.entity.TaskRoleEntity;
import cz.bbn.cerberus.task.persitance.entity.TaskRoleId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;

public interface TaskByRoleRepository extends JpaRepository<TaskRoleEntity, TaskRoleId> {

    @Modifying
    void deleteByIdTaskEntityId(Long taskId);
}
