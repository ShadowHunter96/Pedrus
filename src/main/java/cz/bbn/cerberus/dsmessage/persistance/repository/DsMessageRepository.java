package cz.bbn.cerberus.dsmessage.persistance.repository;

import cz.bbn.cerberus.dsmessage.persistance.entity.DsMessageEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface DsMessageRepository extends JpaRepository<DsMessageEntity, Long> {

    @Modifying
    @Query("update DsMessageEntity entity set entity.viewed = true where id = :id")
    void updateViewed(Long id);

    @Query("select distinct entity.recipientId from DsMessageEntity entity")
    List<String> findRecieverList();

    @Query("select distinct entity.senderName from DsMessageEntity entity")
    List<String> findSenderNameList();
}
