package cz.bbn.cerberus.note.persistance;


import cz.bbn.cerberus.user.persistance.UserEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.Set;


public interface NoteRepository extends JpaRepository<NoteEntity, Long>, JpaSpecificationExecutor<NoteEntity> {

    @Query("select entity.userEntity from NoteEntity entity " +
            "where entity.entityId = :entityId and entity.noteTypeEnum = :noteTypeEnum")
    Set<UserEntity> findNoteUsers(String entityId, String noteTypeEnum);

    @Query("select count(entity) from NoteEntity entity " +
            "where entity.noteTypeEnum = :type and entity.entityId = :objectId and entity.archived = false")
    int getNoteCountByTypeAndObjectId(String type, String objectId);
}
