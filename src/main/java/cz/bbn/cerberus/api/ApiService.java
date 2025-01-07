package cz.bbn.cerberus.api;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.AppUser;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.user.dto.UserDto;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class ApiService {

    private final ListService listService;

    public ApiService(ListService listService) {
        this.listService = listService;
    }

    public List<ItemDto> getCustomerList(String login) throws SystemException {
        UserDto userDto = listService.getUserDtoList().stream()
                .filter(actualUserDto ->
                        actualUserDto.getLogin().equals(login)).findFirst().orElseThrow(() -> new SystemException(ErrorCode.USER_NOT_EXISTS, login));
        List<ItemDto> itemDtoList = new ArrayList<>();
        listService.getSubjectDtoListEditPermission(new AppUser(userDto)).stream()
                .filter(subjectDto -> subjectDto.getCustomer().equals(Boolean.TRUE))
                .forEach(subjectDto ->
                        itemDtoList.add(new ItemDto(subjectDto.getId(), subjectDto.getName())));
        return itemDtoList;
    }

    public List<ItemDto> getItemDtoListByType(String login, String entityType) throws SystemException {
        List<ItemDto> itemDtoList = new ArrayList<>();
        UserDto userDto = listService.getUserDtoList().stream()
                .filter(actualUserDto -> actualUserDto.getLogin().equals(login)).findFirst().orElseThrow(() -> new SystemException(ErrorCode.USER_NOT_EXISTS, login));

        ObjectType objectType = ObjectType.valueOf(entityType);
        switch (objectType){
            case SUBJECT -> listService.getSubjectDtoListEditPermission(new AppUser(userDto))
                        .forEach(subjectDto -> itemDtoList.add(new ItemDto(subjectDto.getId(), subjectDto.getName())));
            case PROJECT -> listService.getProjectDtoListEditPermission(new AppUser(userDto))
                    .forEach(projectDto -> itemDtoList.add(new ItemDto(projectDto.getId(), projectDto.getName())));
            case OFFER -> listService.getOfferDtoListEditPermission(new AppUser(userDto))
                    .forEach(offerDto -> itemDtoList.add(new ItemDto(offerDto.getId(), offerDto.getName())));
            case OPPORTUNITY -> listService.getOpportunityDtoListEditPermission(new AppUser(userDto))
                    .forEach(opportunityDto -> itemDtoList.add(new ItemDto(opportunityDto.getId(), opportunityDto.getName())));
            case CONTRACT -> listService.getContractDtoListEditPermission(new AppUser(userDto))
                    .forEach(contractDto -> itemDtoList.add(new ItemDto(contractDto.getId(), contractDto.getName())));
            default -> {}
        }
        return itemDtoList;
    }
}
