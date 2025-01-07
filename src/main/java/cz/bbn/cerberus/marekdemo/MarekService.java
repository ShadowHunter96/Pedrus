package cz.bbn.cerberus.marekdemo;


import com.vaadin.flow.component.grid.Grid;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.marekdemo.dto.MarekDTO;
import cz.bbn.cerberus.marekdemo.dto.MarekFilterDto;
import cz.bbn.cerberus.marekdemo.entity.MarekEntity;
import cz.bbn.cerberus.marekdemo.factory.MarekFactory;
import cz.bbn.cerberus.marekdemo.repository.MarekDao;
import cz.bbn.cerberus.marekdemo.repository.MarekRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class MarekService {

    private final MarekRepository marekRepository;
    private final MarekDao marekDao;


    public MarekService(MarekRepository taskRepository, MarekDao marekDao) {
        this.marekRepository = taskRepository;
        this.marekDao = marekDao;
    }

    public void showMe() {
        //todo sem si muzes dat metody ktere chces pustit

        List<MarekDTO> taskList = ConvertEntities.fromEntities(marekRepository.findAll(), MarekFactory::fromEntity);

        System.out.println(taskList);
    }

    // returning DTO entities formed from entities in database or repository
    public List<MarekDTO> getAllTaskDtos() {
        return ConvertEntities.fromEntities(marekRepository.findAll(), MarekFactory::fromEntity);
    }


    //StringFilter
    public List<MarekDTO> findAllMarekDtos(String stringfilter) {
        if (stringfilter == null || stringfilter.isEmpty()) {
            return ConvertEntities.fromEntities(marekRepository.findAll(), MarekFactory::fromEntity);
        } else {
            return ConvertEntities.fromEntities(marekRepository.search(stringfilter), MarekFactory::fromEntity);
        }

    }


    public SaveAction<MarekDTO> getSaveAction(Grid<MarekDTO> grid) {
        return (dto, originalDto) -> {
            MarekEntity entity = new MarekEntity();
            MarekFactory.fillEntity(entity, dto);
            marekRepository.save(entity);
            grid.setItems(ConvertEntities.fromEntities(marekRepository.findAll(), MarekFactory::fromEntity));
        };
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            marekRepository.deleteById(Long.valueOf(id));
        };
    }

    public ItemsAction<MarekDTO> getItemsAction() {
        return (query, orderList) -> {
            MarekFilterDto filter = new MarekFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return marekDao.findMarekDtoPage(filter);
        };
    }


}
