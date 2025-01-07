package cz.bbn.cerberus.taskfollowing.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;

import java.util.Set;

public interface TaskFollowingRepository
        extends JpaRepository<TaskFollowingEntity, TaskFollowingId>, JpaSpecificationExecutor<TaskFollowingEntity> {

    Set<TaskFollowingEntity> findAllByIdUserEntityId(Long userId);

    @Modifying
    void deleteByIdUserEntityId(Long userId);
}
