use super::*;

pub fn dominators(cfg: &Cfg) -> Vec<Option<BlockRef>> {
    let mut visited = vec![false; cfg.blocks.len()];
    let mut labels = vec![0; cfg.blocks.len()];
    let mut postorder = Vec::with_capacity(cfg.blocks.len());
    let mut preds: Vec<Vec<BlockRef>> = vec![vec![]; cfg.blocks.len()];
    dfs(cfg, cfg.entry, &mut visited, &mut labels, &mut postorder, &mut preds, None);

    let mut doms: Vec<Option<BlockRef>> = vec![None; cfg.blocks.len()];
    doms[cfg.entry.0] = Some(cfg.entry);

    let mut changed = true;
    while changed {
        changed = false;
        for b in postorder.iter().rev().skip(1) {
            let preds = &*preds[b.0];
            let mut new_idom = preds[0];
            for &p in &preds[1..] {
                if !doms[p.0].is_none() {
                    new_idom = intersect(&labels, &mut *doms, p, new_idom)
                }
            }
            if doms[b.0] != Some(new_idom) {
                doms[b.0] = Some(new_idom);
                changed = true;
            }
        }
    }

    doms
}

fn intersect(labels: &Vec<usize>, doms: &mut [Option<BlockRef>], b1: BlockRef, b2: BlockRef) -> BlockRef {
    let mut finger1 = b1;
    let mut finger2 = b2;
    while finger1 != finger2 {
        while labels[finger1.0] < labels[finger2.0] {
            finger1 = doms[finger2.0].unwrap()
        }
        while labels[finger2.0] < labels[finger1.0] {
            finger2 = doms[finger1.0].unwrap()
        }
    }
    finger1
}

fn dfs(cfg: &Cfg, block: BlockRef, visited: &mut Vec<bool>, labels: &mut Vec<usize>, postorder: &mut Vec<BlockRef>, preds: &mut Vec<Vec<BlockRef>>, pred: Option<BlockRef>) {
    if let Some(pred) = pred {
        preds[block.0].push(pred);
    }

    // if we are visiting this block for the first time
    if !visited[block.0] {
        visited[block.0] = true;

        for succ in cfg.blocks[block.0].successors() {
            dfs(cfg, succ.block, visited, labels, postorder, preds, Some(block));
        }

        labels[block.0] = postorder.len();
        postorder.push(block);
    }
}

/*
pub fn regalloc(cfg: Cfg) -> FxHashMap<Temp, RvRegister> {
    let mut interference = Vec::<InterferenceEntry>::with_capacity(cfg.temps);
    todo!()
}

fn snca(blocks: &[Block], entry: BlockRef, dominator: &mut [BlockRef]) {
    // initialize some vectors and do DFS
    let mut dfs = Vec::with_capacity(blocks.len());
    let mut label = vec![0; blocks.len()];
    let mut semi = vec![0; blocks.len()];
    let mut pred = vec![vec![]; blocks.len()];
    let mut parent = vec![BlockRef(0); blocks.len()];
    dfs_traverse(blocks, &mut dfs, &mut label, &mut semi, &mut pred, &mut parent, entry, 0);

    // iterate over reverse preorder
    for b in dfs[1..].iter().rev() {
        for pred in pred[b.0].iter() {
            //snca_compress(pred)
            semi[b.0] = semi[b.0].min(label[pred.0])
        }
        label[b.0] = semi[b.0]
    }

    let mut idom = parent.clone();

    for b in dfs[1..].iter() {
        while label[idom[b.0].0] > semi[b.0] {
            idom[b.0] = idom[idom[b.0].0]
        }
    }
}

fn snca_compress(label: &mut [usize], ancestor: , v: BlockRef) {

}

fn dfs_traverse(blocks: &[Block], dfs: &mut Vec<BlockRef>, label: &mut [usize], semi: &mut [usize], pred: &mut[Vec<BlockRef>], parent: &mut [BlockRef], b: BlockRef, depth: usize) {
    if depth != 0 && label[b.0] != 0 {
        // already visited
        return
    }

    label[b.0] = dfs.len();
    semi[b.0] = depth;
    dfs.push(b);
    for succ in blocks[b.0].successors() {
        if parent[succ.block.0] == BlockRef(0) {
            parent[succ.block.0] = b
        }
        pred[succ.block.0].push(b);
        dfs_traverse(blocks, dfs, label, semi, pred, parent, succ.block, depth + 1)
    }
}

struct InterferenceEntry {
    neigh: FixedBitSet,
}

pub enum RvRegister {
    X(u8),
    F(u8),
    Stack(u16),
}
*/
