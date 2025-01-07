package cz.bbn.cerberus.note;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.note.dto.NoteDto;
import cz.bbn.cerberus.note.dto.NoteFilterDto;
import cz.bbn.cerberus.note.factory.NoteFactory;
import cz.bbn.cerberus.note.persistance.NoteDao;
import cz.bbn.cerberus.note.persistance.NoteEntity;
import cz.bbn.cerberus.note.persistance.NoteRepository;
import cz.bbn.cerberus.user.persistance.UserEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;
import java.util.Set;

@Service
public class NoteService {

    private static final String OBJECT = "note";

    private final NoteDao noteDao;
    private final NoteRepository noteRepository;
    private final AppLogService appLogService;

    public NoteService(NoteDao noteDao, NoteRepository noteRepository, AppLogService appLogService) {
        this.noteDao = noteDao;
        this.noteRepository = noteRepository;
        this.appLogService = appLogService;
    }

    public Page<NoteDto> findNoteDtoPage(NoteFilterDto filter, List<Sort.Order> orderList) {
        return noteDao.findNoteDtoPage(filter, orderList);
    }

    public NoteDto getNote(Long id) throws SystemException {
        NoteEntity entity = getNoteEntity(id);
        return NoteFactory.fromEntity(entity);
    }

    public boolean noteExists(Long id) {
        return noteRepository.existsById(id);
    }

    @Transactional
    public void saveNote(NoteDto dto) {
        NoteEntity entity = new NoteEntity();
        saveNote(entity, dto);
        appLogService.logInsert(dto, OBJECT);
    }

    @Transactional
    public void updateNote(NoteDto dto, NoteDto originalDto) throws SystemException {
        NoteEntity entity = getNoteEntity(dto.getId());
        saveNote(entity, dto);
        appLogService.logUpdate(dto, originalDto, OBJECT);
    }

    @Transactional
    public void deleteNote(Long id) throws SystemException {
        if (!noteRepository.existsById(id)) {
            throw new SystemException(ErrorCode.NOTE_NOT_EXISTS);
        }
        noteRepository.deleteById(id);
        appLogService.logDelete(String.valueOf(id), OBJECT);
    }

    private void saveNote(NoteEntity entity, NoteDto dto) {
        NoteFactory.fillEntity(entity, dto);
        noteRepository.save(entity);
    }

    private NoteEntity getNoteEntity(Long id) throws SystemException {
        return noteRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.NOTE_NOT_EXISTS));
    }

    public Set<UserEntity> findNoteUsers(String entityId, NoteTypeEnum noteTypeEnum) {
        return noteRepository.findNoteUsers(entityId, noteTypeEnum.name());
    }

    public int getNoteCountByTypeAndObjectId(NoteTypeEnum type, String objectId) {
        return noteRepository.getNoteCountByTypeAndObjectId(type.name(), objectId);
    }

    public int getNoteCountByType(NoteFilterDto filter) {
        return noteDao.getNoteCountByFilter(filter);
    }
}
