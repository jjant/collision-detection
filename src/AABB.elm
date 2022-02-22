module AABB exposing
    ( AABB
    , center
    , projectLocalPoint
    )

import Circle exposing (PointProjection)
import Util
import Vec2 exposing (Vec2)


type alias AABB =
    { min : Vec2
    , max : Vec2
    }


center : AABB -> Vec2
center { min, max } =
    Vec2.midpoint min max


projectLocalPoint : Vec2 -> AABB -> PointProjection
projectLocalPoint localPoint { min, max } =
    let
        minPt =
            Vec2.sub min localPoint

        ptMax =
            Vec2.sub localPoint max

        shift =
            Vec2.sub (Util.componentWiseMax minPt Vec2.zero)
                (Util.componentWiseMax ptMax Vec2.zero)

        inside =
            shift == Vec2.zero
    in
    if not inside then
        { isInside = False
        , point = Vec2.add localPoint shift
        }

    else
        -- else if solid
        -- TODO: Decide if we should handle solids
        -- TODO: Implement the rest of the algorithm (rust code below)
        --
        { isInside = True
        , point = localPoint
        }



-- else
-- let _max: Real = Bounded::max_value();
-- let mut best = -_max;
-- let mut is_mins = false;
-- let mut best_id = 0;
-- for i in 0..DIM {
--     let mins_pt_i = mins_pt[i];
--     let pt_maxs_i = pt_maxs[i];
--     if mins_pt_i < pt_maxs_i {
--         if pt_maxs[i] > best {
--             best_id = i;
--             is_mins = false;
--             best = pt_maxs_i
--         }
--     } else if mins_pt_i > best {
--         best_id = i;
--         is_mins = true;
--         best = mins_pt_i
--     }
-- }
-- let mut shift: Vector<Real> = na::zero();
-- if is_mins {
--     shift[best_id] = best;
-- } else {
--     shift[best_id] = -best;
-- }
-- (inside, pt + shift, shift)
