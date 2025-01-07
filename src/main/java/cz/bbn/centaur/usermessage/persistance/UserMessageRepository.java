package cz.bbn.cerberus.usermessage.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDateTime;

public interface UserMessageRepository
        extends JpaRepository<UserMessageEntity, Long>, JpaSpecificationExecutor<UserMessageEntity> {

    @Modifying
    @Query("update UserMessageEntity entity " +
            "set entity.viewed = true " +
            "where entity.id = :id")
    void saveViewed(Long id);

    @Query("select count(entity.id) from UserMessageEntity entity " +
            "where entity.userId = :userId and entity.viewed = false and entity.dueDate >= :date ")
    int getCountByUserAndViewed(Long userId, LocalDateTime date);
}
