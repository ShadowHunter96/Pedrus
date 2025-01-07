package cz.bbn.cerberus.usermessage;

import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.usermessage.dto.UserMessageDto;
import cz.bbn.cerberus.usermessage.dto.UserMessageFilterDto;
import cz.bbn.cerberus.usermessage.factory.UserMessageFactory;
import cz.bbn.cerberus.usermessage.persistance.UserMessageDao;
import cz.bbn.cerberus.usermessage.persistance.UserMessageEntity;
import cz.bbn.cerberus.usermessage.persistance.UserMessageRepository;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Random;
import java.util.UUID;


@Service
public class UserMessageService {

    private final UserMessageDao userMessageDao;
    private final UserMessageRepository userMessageRepository;
    private final Random rand;

    public UserMessageService(UserMessageDao userMessageDao, UserMessageRepository userMessageRepository)
            throws NoSuchAlgorithmException {
        this.userMessageDao = userMessageDao;
        this.userMessageRepository = userMessageRepository;
        rand = SecureRandom.getInstanceStrong();
    }

    public Page<UserMessageDto> findUserMessageDtoPage(UserMessageFilterDto filterDto) {
        return userMessageDao.findUserMessageDtoPage(filterDto);
    }

    public UserMessageDto findLastUnViewed(UserMessageFilterDto userMessageFilterDto) {
        return userMessageDao.findLastUnviewed(userMessageFilterDto);
    }


    public int getUserMessageCount() {
        Long userId = SecurityUtils.getCurrentUserId();
        if (userId == null) {
            return 0;
        }
        return userMessageRepository.getCountByUserAndViewed(userId, LocalDateTime.now());
    }

    @Transactional
    public void saveViewed(Long id) {
        userMessageRepository.saveViewed(id);
    }

    @Transactional
    public void saveUserMessage(UserMessageDto userMessageDto) {
        UserMessageEntity userMessageEntity = UserMessageFactory.fromDto(userMessageDto);
        userMessageRepository.save(userMessageEntity);
    }

    @Transactional
    public void randMessageToUser(Long userId) {
        UserMessageDto userMessageDto = new UserMessageDto();
        userMessageDto.setMessage("Test message ".concat(UUID.randomUUID().toString()));
        userMessageDto.setUserId(userId);
        userMessageDto.setType(MessageType.values()[rand.nextInt(MessageType.values().length)]);
        userMessageDto.setPriority(rand.nextBoolean());
        userMessageDto.setDueDate(LocalDateTime.now().plusDays(rand.nextInt(1, 7)));
        userMessageDto.setObjectId("");
        int objectType = rand.nextInt(UserMessageObjectType.values().length);
        userMessageDto.setObjectType(UserMessageObjectType.values()[objectType == 0 ? 1 : objectType]);
        userMessageDto.setViewed(false);
        saveUserMessage(userMessageDto);
    }
}
