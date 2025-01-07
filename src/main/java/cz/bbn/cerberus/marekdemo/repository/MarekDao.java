package cz.bbn.cerberus.marekdemo.repository;

import cz.bbn.cerberus.marekdemo.dto.MarekDTO;
import cz.bbn.cerberus.marekdemo.dto.MarekFilterDto;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Component;

/**
 * Created by marek.vu on 09.10.2023.
 */
@Component
public class MarekDao {

    public Page<MarekDTO> findMarekDtoPage(MarekFilterDto filter) {
        return null;
    }
}
